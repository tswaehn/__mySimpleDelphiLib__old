unit textFileWriter;

interface
uses Chart,SysUtils, Dialogs ;

type
  TTextFileWriter = class (TObject)
    constructor Create( filename_:string );
    destructor Destroy(); override;

    procedure writeLn( line:string );
    procedure writeDouble( x,y: double;c:string );
    
  private
    filename:string;
    hFile: textfile;

  end;
implementation

constructor TTextFileWriter.Create(filename_:string );
begin
  inherited Create();
  filename:= filename_;
  assignFile(hFile, filename);
  {$I+} 
  rewrite(hFile);
  {$I-} 
  if (IOResult <> 0) then begin
    showMessage('error '+inttostr(IOResult));
  
  end;


end;

destructor TTextFileWriter.destroy;
begin
  closeFile(hFile);
  
  inherited Destroy;
end;

procedure TTextFileWriter.writeLn( line:string );
begin
  write(hFile,line +#13 +#10);
end;

procedure TTextFileWriter.writeDouble( x,y: double;c:string );
begin
  write(hFile,
      FloatToStrF(x,ffFixed,10,3) + #9
      + FloatToStrF(y,ffFixed,10,3) +#9
      + c +#13 +#10 );
end;

end.

