unit enumConverter;

interface

uses classes, System.TypInfo;

// https://stackoverflow.com/questions/2472487/converting-an-string-to-a-enum-type-using-tvalue

type TEnumConverter = class
public
  class function EnumToInt<T>(const EnumValue: T): Integer;
  class function EnumToString<T>(EnumValue: T): string;
  class procedure StringToEnum<T>(strValue:String; var enValue:T);
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


class procedure TEnumConverter.StringToEnum<T>(strValue: String; var enValue:T);
   var Tipo : PTypeInfo;
       Temp:Integer;
       PTemp : pointer;
begin
    Tipo := TypeInfo(T);
    Temp := GetEnumValue(Tipo, strValue);
    PTemp := @Temp;
    enValue := T(PTemp^);
end;


end.
