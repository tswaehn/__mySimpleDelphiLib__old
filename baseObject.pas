unit baseObject;

interface
uses classes, sysutils;

type
  TBaseObject = class(TObject)

    constructor Create();
    destructor Destroy(); overload; virtual;

  end;


implementation

  var globalObjectDirectory: TStringList;


constructor TBaseObject.Create;
var index: integer;
    className: string;
    count: integer;
begin
  inherited Create();
  (*
  if (globalObjectDirectory = nil) then begin
    globalObjectDirectory:= TStringList.Create;
  end;

  className:= self.ClassName;
  // add me to the list
  index:= globalObjectDirectory.IndexOfName(className);

  if (index >= 0) then begin
    count:= StrToInt( globalObjectDirectory.ValueFromIndex[index] );
    INC(count);
    globalObjectDirectory.ValueFromIndex[index]:= intToStr( count );
  end else begin
    count:= 1;
    globalObjectDirectory.AddPair( classname, intToStr(count) );
  end;
          *)
end;

destructor TBaseObject.Destroy;
var index: integer;
    className: string;
    count: integer;
begin
(*
  className:= self.ClassName;
  // add me to the list
  index:= globalObjectDirectory.IndexOfName(className);

  if (index >= 0) then begin
    count:= StrToInt( globalObjectDirectory.ValueFromIndex[index] );
    DEC(count);
    globalObjectDirectory.ValueFromIndex[index]:= intToStr( count );
  end else begin
    count:= 0;
    globalObjectDirectory.AddPair( classname, intToStr( count ) );
  end;

*)
  inherited Destroy();
end;

end.
