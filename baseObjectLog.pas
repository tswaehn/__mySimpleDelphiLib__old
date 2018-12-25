unit baseObjectLog;

interface
uses Classes, baseObject;

(*
    this is the base class for objects that can log something

*)
type
  TMyLog = procedure(text: string) of object;


type
  TBaseObjectLog = class (TBaseObject)


  protected
      procedure log(text:string);

  private
    onLog: TMyLog;

  protected
    procedure setLogProcedure( logger:TMyLog );

  public
    property logger:TMyLog read onLog write onLog;





  end;
implementation

procedure TBaseObjectLog.log(text:string);
var
    className: string;
begin

  if (assigned(onLog)) then begin
    className:= self.ClassName;
    onLog( className + '>' + text );
  end;
end;

procedure TBaseObjectLog.setLogProcedure( logger:TMyLog );
begin
  onLog:= logger;
end;

end.
