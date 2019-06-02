unit csvHandlerSimple;

interface
uses csvHandler,
     Classes, Dialogs;

(*
    TCsvHandler simple is a special CSV handler that reads files without quotes

      example and comparision:
        [TCsvHandlerSimple]
          200,AlignByDieOrigin failed,ODIN,Scan Alignment failed,1,,1,,

        [TCsvHandler]
          "200","AlignByDieOrigin failed","ODIN","Scan Alignment failed","1",,"1",,


    that means, the simple handler cannot handle all chars. still there are files
    like this that need to be read.


    Why is there a special handler and we dont have the automatic detection?
      - answer is simple: performance
      - we want to read large files fast and reliable - so we clearly define the
        input and use specific handlers if needed.
*)

type
  TCsvHandlerSimple = class (TCsvHandler)

    // override the test cases to make sure the handler is working
    procedure runTestCases(); override;

    // override the parsing function
    function parseSingleLine(line: string): TStringList; override;

  end;

implementation

// override
procedure TCsvHandlerSimple.runTestCases();
var testCases: TStringList;
    i: Integer;
    temp:TStringList;
begin
  testCases:= TStringList.Create;
  // ok to have comma in the last cell
  testCases.Add('LogEntry,Warning,0,Cancelled,Task:TTCFalconScanJobTask,Equipment Manager (WSNB1),WSNB1,8064,2884,13.09.2018 04:17:35.122,,,also comma, here;this is text;or something else');
  // ok to have single separator somewhere in quotes + have spaces between quote and separator
  testCases.Add('Log ; Entry, Warning,0,Cancelled,Task:TTCFalconScanJobTask,Equipment Manager (WSNB1),WSNB1,8064,2884,13.09.2018 04:17:35.122,,,,');
  // ok to have single quotes somewhere in quotes
  testCases.Add('Log;Entry,Warning,0,Cancelled,Task:TTCFalconScanJobTask,Equipment Manager (WSNB1),WSNB1,8064,2884,13.09.2018 04:17:35.122,,,,x');
  // ok to ignore all non - quoted text - means ok to ignore  x
  testCases.Add('Log; ;Entry,Warning,0,Cancelled,Task:TTCFalconScanJobTask,Equipment Manager (WSNB1),WSNB1,8064,2884,13.09.2018 04:17:35.122,,,,');
  // ok to have empty fields without quotes - will be interpreted as full field
  testCases.Add(',,0,Cancelled,Task:TTCFalconScanJobTask,Equipment Manager (WSNB1),WSNB1,8064,2884,13.09.2018 04:17:35.122,,,,');
  // ok to have empty fields in the middle
  testCases.Add('LogEntry,Warning,,Cancelled,Task:TTCFalconScanJobTask,Equipment Manager (WSNB1),WSNB1,8064,2884,13.09.2018 04:17:35.122,,,,');
  // ok to have empty quotes at the start or end
  testCases.Add(',Warning,,Cancelled,Task:TTCFalconScanJobTask,Equipment Manager (WSNB1),WSNB1,8064,2884,13.09.2018 04:17:35.122,,,,');
  // ok to have empty fields separated by spaces
  testCases.Add('LogEntry,,,Task: Post Processing - SubstrateSide_BackSide Lane=0:Maximum defect count 477 per '+'image was reached for Condensed Defect Image,Task: Post Processing - SubstrateSide_BackSide Lane=0,Equipment Manager (WPNB10),WSNB1,7560,4588,13.09.2018 03:54:32.606,,,,');
  // ok to have lines that are longer than 255 characters
  testCases.Add('LogEntry,Warning,0,Task: Post Processing - SubstrateSide_BackSide Lane=0:Maximum defect count 477 per '+
                'image was reached for Condensed Defect Image,Task: Post Processing - SubstrateSide_BackSide Lane=0,Equipment Manager (WPNB10),WSNB1,7560,4588,13.09.2018 03:54:32.606,,,,');
  // ok to have single quote lines '
  testCases.Add('LogEntry,Verbose,0,Driver [Albatross30 Galil Controller 2.0]:Settings Source Code: ''''------------------------------------------------------------ ''''Contact persons: M. Otto/S. Ginka/B. Zimmer - HSEB Dresden GmbH ''''Version 2.22 ''''ODIN 301'+' (optional 3D) ''''2017/07/05 ''''------------------------------------------------------------ ''''History: ''''2013/12/06 '''' - target_pos direkt auf turnaround point '''' - zmove bereits nach dem'+'letzen trigger-shot ''''2011/12/15 '''' - in motion flag ''''2011/12/12 '''' - OC issue 2^16 ''''2011/11/01 '''' - variable position window ''''2011/10/11 '''' - init of all variables '''' - removed AP[X/Y] in trigger routine ''''2011/09/29 '''' '+'OCX=_TPX;0 ENDIF EN  ,Driver [Albatross30 Galil Controller 2.0],Tool Control,C38AMI920,2820,6868,16.09.2018 03:38:36.839,,,,' );
  // ok to have quote followed by comma but not followed by quote
  testCases.Add('LogEntry,Warning,0,Cancelled,Task: Post Processing - SubstrateSide_BackSide Lane=0:Maximum defect count 477,Equipment Manager (WPNB10),WSNB1,7560,4588,13.09.2018 03:54:32.606,,,,');


  for i := 0 to testCases.Count-1 do begin
    temp:= self.parseSingleLine( testCases.Strings[i] );

    if (temp.Count <> 14) then begin
      showmessage('test failed');
    end;
    temp.Free;
  end;


  testCases.Free();
end;

// override
function TCsvHandlerSimple.parseSingleLine(line: string): TStringList;
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

    a:= 1;
    for i := 0 to (count-1) do begin
      idx:= splitList[i];
      if (idx > 0) then begin
        len:= idx-a;
        if (len > 0) then begin
          temp:= copy(line, a, len);
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
      temp:= copy(line, a, len);
    end else begin
      temp:= '';
    end;
    res.Add(temp);

    result:= res;
end;

end.
