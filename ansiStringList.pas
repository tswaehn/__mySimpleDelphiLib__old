unit ansiStringList;

interface
uses classes, sysutils;

type
  TAnsiStringList= class(TList)
    constructor Create();

    procedure Add( AString: AnsiString );
  end;

implementation

constructor TAnsiStringList.Create();
begin
  inherited Create();

end;

procedure TAnsiStringList.Add( AString: AnsiString );
var count: integer;
begin
  self.Insert( self.Count , Pointer( AString ) );
end;

end.
