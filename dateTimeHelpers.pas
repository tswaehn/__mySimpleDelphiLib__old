unit dateTimeHelpers;

interface
uses SysUtils, Classes,dateUtils;

  function MySQLStringToDateTime( str:string ):TDateTime;

  function getTotalSeconds( duration:TDateTime ):double;

  function strToTimestampInt( str: string ): Int64;
  function timestampIntToStr(timestampInt:int64):string;

  function timstampIntToTdateTime( timestampInt:int64 ): TDateTime;
  function TdateTimeToTimestampInt( dateTime:TDateTime ): int64;


implementation

// converts MySQL time into TDateTime
// ex. 2018-07-06 07:08:13 => TDateTime
function MySQLStringToDateTime( str:string ):TDateTime;
var
  fs: TFormatSettings;
  sl: TStringList;
  date: TStringList;
  reOrderedStr:string;
begin
  // step 1: split in date and time
  sl := TStringList.Create;
  sl.Delimiter := ' ';
  sl.DelimitedText := str;
  if (sl.count <> 2) then begin
    raise Exception.Create('invalid string for MySQLStringToDateTime');
  end;
  // step 2: get pieces of date
  date:= TStringList.Create;
  date.Delimiter:='-';
  date.DelimitedText:= sl.Strings[0];

  // step 3: re-order for clear date
  reORderedStr:= date.Strings[2]+'.'+date.Strings[1]+'.'+date.Strings[0]+' '+ sl.Strings[1];

  GetLocaleFormatSettings(1031, fs);
  Result := StrToDateTime(reORderedStr, fs);
end;

// convert a duration given in TDateTime into float value of seconds
function getTotalSeconds( duration:TDateTime ):double;
var
  year,dayOfYear,
  hour,min,sec,msec:word;
begin
  DecodeTime(duration, hour, min, sec, msec );
  DecodeDateDay(duration, year, dayOfYear );


  year:= year - 1899;
  dayOfYear:= dayOfYear - 364; // 0.0 correlates to 30.12.1899 12:00

  result:= 0;
  result:= result + msec/1000;
  result:= result + sec;
  result:= result + min*60;
  result:= result + hour*3600;
  result:= result + dayOfYear*24*3600;
  result:= result + year*365*24*3600;
end;

// decode of "14.09.2018 10:21:33.784" including msec
function strToTimestampInt( str: string ): Int64;
  var
  dateTime:TDateTime;
  glFmtSet: TFormatSettings;
  timestampInt: Int64;
begin

  glFmtSet:= TFormatSettings.Create();
  glFmtSet.DecimalSeparator := '.';

  dateTime:= StrToDateTime( str, glFmtSet );

  timestampInt:= TdateTimeToTimestampInt( dateTime );
  //dateStr:= FormatDateTime('dd/mm/yy hh:nn:ss.zzz', timestampInt / (1000*3600*24));

  result:= timestampInt;
end;

function timestampIntToStr(timestampInt:int64):string;
begin
  result:= FormatDateTime('dd/mm/yy hh:nn:ss.zzz', timstampIntToTdateTime(timestampInt));
end;


function timstampIntToTdateTime( timestampInt:int64 ): TDateTime;
begin

  result:= timestampInt / (1000*3600*24);
end;

function TdateTimeToTimestampInt( dateTime:TDateTime ): int64;
begin
  result:= round( Double(dateTime) * (1000*3600*24));
end;
end.
