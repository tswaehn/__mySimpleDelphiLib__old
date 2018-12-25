unit enumConverter;

interface

uses classes, System.TypInfo;

type TEnumConverter = class
public
  class function EnumToInt<T>(const EnumValue: T): Integer;
  class function EnumToString<T>(EnumValue: T): string;
end;


implementation

class function TEnumConverter.EnumToInt<T>(const EnumValue: T): Integer;
begin
  Result := 0;
  Move(EnumValue, Result, sizeOf(EnumValue));
end;

class function TEnumConverter.EnumToString<T>(EnumValue: T): string;
begin
  Result := GetEnumName(TypeInfo(T), EnumToInt(EnumValue));
end;


end.
