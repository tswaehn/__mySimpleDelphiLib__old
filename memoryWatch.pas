unit memoryWatch;

interface
uses  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, psAPI;

type
  TMemoryWatch = class(TObject)
    constructor Create();
    procedure start();
    function finish(): int64;

    private
      function memoryUsed():int64;
      function CurrentProcessMemory: Cardinal;

    private
      mStartValue: int64;
      mDelta: int64;
  end;

implementation

constructor TMemoryWatch.Create;
begin
  inherited Create();
  start();
end;

function TMemoryWatch.MemoryUsed: int64;
var
    st: TMemoryManagerState;
    sb: TSmallBlockTypeState;
begin
    GetMemoryManagerState(st);
    result :=  st.TotalAllocatedMediumBlockSize * st.AllocatedMediumBlockCount
              + st.TotalAllocatedLargeBlockSize * st.AllocatedLargeBlockCount;
  (*
    result := st.TotalAllocatedMediumBlockSize + st.TotalAllocatedLargeBlockSize;
    for sb in st.SmallBlockTypeStates do begin
        result := result + sb.UseableBlockSize * sb.AllocatedBlockCount;
    end;
    *)
end;

function TMemoryWatch.CurrentProcessMemory: Cardinal;
var
  MemCounters: TProcessMemoryCounters;
begin
  MemCounters.cb := SizeOf(MemCounters);
  if GetProcessMemoryInfo(GetCurrentProcess, @MemCounters, SizeOf(MemCounters)) then begin
    Result := MemCounters.WorkingSetSize
  end else begin
    RaiseLastOSError;
  end;
end;


procedure TMemoryWatch.start();
begin
  mStartValue:= CurrentProcessMemory();
end;

function TMemoryWatch.finish(): int64;
begin
  mDelta:= CurrentProcessMemory() - mStartValue;
  result:= mDelta;
end;

end.
