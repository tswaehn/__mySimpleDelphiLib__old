unit csvHandler;

interface
uses Classes, SysUtils, Dialogs,
    baseObject, mytypes, fileHelpers;


type
  TCsvHandler = class(TBaseObject)
    constructor Create( filename:string;forceRecreate:boolean=false );
    destructor destroy(); override;

    procedure rewind();
    function readLine():TStringList;
    procedure writeLine( line: TStringListPtr );

    function isEmpty():boolean;

    protected
      // can do override for special CSV styles/types
      procedure runTestCases();  virtual;
      function parseSingleLine(line: string): TStringList; virtual;

    private
      function getLine():string;
      procedure readBuffer();


      function prepareWriteLine( line:TStringListPtr ): string;

      function copyAndStripQuotes( line:string; index:integer; cpyLen:integer):string;

      procedure addToDisposeList( stringListPtr: TStringListPtr );

    private
      fileStream:TFileStream;
      buffer:AnsiString;

      mDisposeList: array of TStringListPtr;
      mDisposeListCount: integer;


  end;

const BUFFER_SIZE: integer = 1000;
const LE: AnsiString = #13+#10;
const CSV_DELIMITER: char = ',';
const CSV_QUOTE: char = '"';
const CSV_SPACE: char = #32;

implementation

constructor TCsvHandler.Create(filename: string;forceRecreate:boolean=false);
begin
  inherited Create();

  // check if folder to file exists, if not, create it
  checkFolderIfNotExistsCreateIt( filename );

  // create if non-existent
  if (not fileexists(filename)) or (forceRecreate) then begin
    try
      fileStream:= TFileStream.Create(filename, fmCreate, fmShareExclusive );
    except
      raise Exception.Create('Cannot create file '+filename);
    end;
    fileStream.Destroy;
  end;

  // open exclusive for read/write
  try
    fileStream:= TFileStream.Create(filename, fmOpenReadWrite, fmShareExclusive );
  except
    raise Exception.Create('Cannot open file '+filename);
  end;

  // make sure the csv can handle all standard cases
  self.runTestCases();

  mDisposeListCount:= 0;
  setlength(mDisposeList, mDisposelistCount);

end;

destructor TCsvHandler.destroy;
var
  i: Integer;
begin
  if fileStream <> nil then begin
    fileStream.Destroy;
  end;

  for i := 0 to mDisposeListCount-1 do begin
    mDisposeList[i].Destroy;
    dispose( mDisposeList[i] );
    mDisposeList[i]:= nil;
  end;
  mDisposeListCount:= 0;
  setlength(mDisposeList, 0);

  inherited destroy;
end;

(*

    examples for testing:

*)
procedure TCsvHandler.runTestCases();
var testCases: TStringList;
    i: Integer;
    temp:TStringList;
begin
  testCases:= TStringList.Create;
  // ok to have comma in the last cell
  testCases.Add('"LogEntry","Warning","0","Cancelled","Task:TTCFalconScanJobTask","Equipment Manager (WSNB1)","WSNB1","8064","2884","13.09.2018 04:17:35.122",,,"also comma, here","this is text,or something else"');
  // ok to have single separator somewhere in quotes + have spaces between quote and separator
  testCases.Add('"Log , Entry"," Warning","0","Cancelled","Task:TTCFalconScanJobTask","Equipment Manager (WSNB1)","WSNB1","8064","2884","13.09.2018 04:17:35.122",,,,');
  // ok to have single quotes somewhere in quotes
  testCases.Add('"Log,Entry","War"n"ing","0","Cancelled","Task:TTCFalconScanJobTask","Equipment Manager (WSNB1)","WSNB1","8064","2884","13.09.2018 04:17:35.122",,,,x');
  // ok to ignore all non - quoted text - means ok to ignore " x "
  testCases.Add('"Log, ,Entry","Warning","0","Cancelled","Task:TTCFalconScanJobTask","Equipment Manager (WSNB1)","WSNB1","8064","2884","13.09.2018 04:17:35.122",,,,');
  // ok to have empty fields without quotes - will be interpreted as full field
  testCases.Add(',,"0","Cancelled","Task:TTCFalconScanJobTask","Equipment Manager (WSNB1)","WSNB1","8064","2884","13.09.2018 04:17:35.122",,,,');
  // ok to have empty fields in the middle
  testCases.Add('"LogEntry","Warning",,"Cancelled","Task:TTCFalconScanJobTask","Equipment Manager (WSNB1)","WSNB1","8064","2884","13.09.2018 04:17:35.122",,,,');
  // ok to have empty quotes at the start or end
  testCases.Add('"","Warning",,"Cancelled","Task:TTCFalconScanJobTask","Equipment Manager (WSNB1)","WSNB1","8064","2884","13.09.2018 04:17:35.122",,,,""');
  // ok to have empty fields separated by spaces
  testCases.Add('"LogEntry",,,"Task: Post Processing - SubstrateSide_BackSide Lane=0:Maximum defect count "477" per '+'image was reached for "Condensed Defect Image"","Task: Post Processing - SubstrateSide_BackSide Lane=0","Equipment Manager (WPNB10)","WSNB1","7560","4588","13.09.2018 03:54:32.606",,,,');
  // ok to have lines that are longer than 255 characters
  testCases.Add('"LogEntry","Warning","0","Task: Post Processing - SubstrateSide_BackSide Lane=0:Maximum defect count "477" per '+
                'image was reached for "Condensed Defect Image"","Task: Post Processing - SubstrateSide_BackSide Lane=0","Equipment Manager (WPNB10)","WSNB1","7560","4588","13.09.2018 03:54:32.606",,,,');
  // ok to have single quote lines '
  testCases.Add('"LogEntry","Verbose","0","Driver [Albatross30 Galil Controller 2.0]:Settings Source Code: "''''------------------------------------------------------------ ''''Contact persons: M. Otto/S. Ginka/B. Zimmer - HSEB Dresden GmbH ''''Version 2.22 ''''ODIN 301'+' (optional 3D) ''''2017/07/05 ''''------------------------------------------------------------ ''''History: ''''2013/12/06 '''' - target_pos direkt auf turnaround point '''' - zmove bereits nach dem'+'letzen trigger-shot ''''2011/12/15 '''' - in motion flag ''''2011/12/12 '''' - OC issue 2^16 ''''2011/11/01 '''' - variable position window ''''2011/10/11 '''' - init of all variables '''' - removed AP[X/Y] in trigger routine ''''2011/09/29 '''' '+'OCX=_TPX,0 ENDIF EN  "","Driver [Albatross30 Galil Controller 2.0]","Tool Control","C38AMI920","2820","6868","16.09.2018 03:38:36.839",,,,' );
  // ok to have quote followed by comma but not followed by quote
  testCases.Add('"LogEntry","Warning","0","Cancelled","Task: Post Processing - "SubstrateSide_BackSide", Lane=0:Maximum defect count "477"","Equipment Manager (WPNB10)","WSNB1","7560","4588","13.09.2018 03:54:32.606",,,,');


  for i := 0 to testCases.Count-1 do begin
    temp:= self.parseSingleLine( testCases.Strings[i] );

    if (temp.Count <> 14) then begin
      showmessage('test failed');
    end;
    temp.Free;
  end;


  testCases.Free();
end;


function TCsvHandler.copyAndStripQuotes( line:string; index:integer; cpyLen:integer):string;
var temp:string;
  i: Integer;
begin
  if (line[1] = CSV_QUOTE) then begin
    INC(index);
    DEC(cpyLen);
  end;
  if (line[index+cpyLen-1] = CSV_QUOTE) then begin
    DEC(cpyLen);
  end;

  temp:= copy(line, index, cpyLen);
  result:= temp;
end;


function TCsvHandler.parseSingleLine(line: string): TStringList;
var
    res: TStringList;
    temp: string;
    i:integer;
    len:integer;
    idx: integer;
    offset: integer;
    a: integer;
    count: integer;
    splitList: array of integer;
begin
    // create a stringlist and config
    res:= TStringList.Create( dupIgnore, false, true );

    // find all split points -- CSV_DELIMITER
    offset:= 1;
    count:= 0;
    repeat
      idx:= pos(CSV_DELIMITER, line, offset );
      if (idx > 0) then begin
        offset:= idx+1;
        INC( count );
        setlength( splitList, count );
        splitList[count-1]:= idx;
      end;
    until (idx=0);

    // allowed splits are  ","| ,,
    // not allowed are spaces  " ," | " , " | , , |
    // now check carefully each split point for proper , set incorrect split to "-1"
    // before and after split are only CSV_DELIMITER and CSV_QUOTE allowed => else incorrect split
    for i := 0 to count-1 do begin
      idx:= splitList[i];

      // check in front of split
      if (idx > 1) then begin
        if (line[idx-1] <> CSV_QUOTE) and (line[idx-1] <> CSV_DELIMITER) then begin
          // mark split incorrect
          splitList[i]:= -1;
          continue;
        end;
      end;

      // check after split
      if (idx<(length(line)-1)) then begin
        if (line[idx+1] <> CSV_QUOTE) and (line[idx+1] <> CSV_DELIMITER) then begin
          // mark split incorrect
          splitList[i]:= -1;
          continue;
        end;
      end;
    end;

    a:= 1;
    for i := 0 to (count-1) do begin
      idx:= splitList[i];
      if (idx > 0) then begin
        len:= idx-a;
        if (len > 0) then begin
          temp:= copyAndStripQuotes(line, a, len);
        end else begin
          temp:= '';
        end;
        a:= idx+1;
        res.Add( temp );
      end;
    end;

    // copy the rest after last CSV_DELIMITER
    len:= length(line) - (a-1);
    if (len > 0) then begin
      temp:= copyAndStripQuotes(line, a, len);
    end else begin
      temp:= '';
    end;
    res.Add(temp);

    result:= res;
end;

procedure TCsvHandler.addToDisposeList( stringListPtr: TStringListPtr );
begin
  INC(mDisposeListCount);
  setlength(mDisposeList, mDisposeListCount);
  mDisposeList[mDisposeListCount-1]:= stringListPtr;
end;

function TCsvHandler.readLine():TStringList;
var
  res:TStringList;
  line: string;
begin
  res:= nil;

  // get a new line from CSV
  line:= getLine();
  if length(line) > 0 then begin
    // create a stringlist and config
    res:= self.parseSingleLine( line );
  end;

  result:= res;
end;


(*
    typically there is an option of using TStringList directly
    HOWEVER: TStringList cannot consequently quote all fields

    so this is my workaround

*)
function TCsvHandler.prepareWriteLine( line:TStringListPtr ): string;
var
  i: Integer;
  res:string;
begin
  //
  res:='';

  // consequently quote chars
  for i := 0 to line.Count-1 do begin
    res:= res + CSV_QUOTE + line.Strings[i]+ CSV_QUOTE;
    if (i<(line.Count-1)) then begin
      res:= res + CSV_DELIMITER;
    end;
  end;

  res:= res + LE;

  result:=res;
end;

procedure TCsvHandler.writeLine( line: TStringListPtr );
var
  tempStr:AnsiString;
  count:integer;
  byteArray:TBytes;
begin

  // convert TStringList to CSV line
  tempStr:= prepareWriteLine(line);

  // write line to file
  count:= length(tempStr);
  fileStream.WriteBuffer( Pointer(tempStr)^, count  );

end;

procedure TCsvHandler.rewind;
begin
  fileStream.Seek(0, soFromBeginning);
  buffer:= '';
end;


function TCsvHandler.getLine():string;
var byteArray: TBytes;
    AnsiStr: AnsiString;
    line:ansistring;
    count:integer;
    idx:integer;
begin
  // set size of buffer for reading
  setLength(byteArray, BUFFER_SIZE);
  // load data until finding LE or EOF
  repeat
    // check if in buffer is still a full line
    idx:= pos(LE, buffer, 1);
    if (idx > 0) then begin
      break;
    end;

    // else read more from file
    count:= fileStream.Read(byteArray,BUFFER_SIZE);
    SetString(AnsiStr, PAnsiChar(@byteArray[0]), count);
    buffer:= buffer + AnsiStr;

  until (count = 0) or (idx>0);

  // found LE
  if (idx > 0) then begin
    line:= Copy(buffer, 1, idx-1);
    delete(buffer,1, idx-1 + length(LE) );

    result:= line;
    exit;
  end;

  // EOF
  if (count < BUFFER_SIZE) then begin
    line:= buffer;
    buffer:= '';
    result:= line;
    exit;
  end;
end;

function TCsvHandler.isEmpty():boolean;
begin
  if (fileStream.Size = 0) then begin
    result:= true;
  end else begin
    result:= false;
  end;
end;

procedure TCsvHandler.readBuffer;
var buf:TBytes;
begin
  setLength(buf, BUFFER_SIZE);
  fileStream.Read(buf,BUFFER_SIZE);
end;

end.
