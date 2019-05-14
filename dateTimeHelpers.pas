unit dateTimeHelpers;

interface
uses SysUtils, Classes, dateUtils, Dialogs;

  // MySql <=> TDateTime
  function MySQLStringToDateTime( str:string ):TDateTime;
  function TDateTimeToMySQLString( dateTime: TDateTime ): string;

  function getTotalSeconds( duration:TDateTime ):double;

  // String <=> INT64
  function strToTimestampInt( str: string ): Int64;
  function timestampIntToStr(timestampInt:int64):string;

  // TDateTime <=> INT64
  function timstampIntToTdateTime( timestampInt:int64 ): TDateTime;
  function TdateTimeToTimestampInt( dateTime:TDateTime ): int64;

  // String <=> TDateTime
  function strToTDateTime( str: string ): TDateTime;
  function strTimeToTDateTime( str: string ): TDateTime;
  function TDateTimeToStr( dateTime:TDateTime ): string;
  function TDateTimeToNiceStr( dateTime:TDateTime ): string;
  function TDateTimeToTimeStr( dateTime:TDateTime ): string;


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

// converts TDateTime into MySQL time
// ex. TDateTime => 2018-07-06 07:08:13
function TDateTimeToMySQLString( dateTime: TDateTime ): string;
var
  formattedDateTimeStr: string;
begin
  //
  DateTimeToString( formattedDateTimeStr,    'yyyy-mm-dd hh:nn:ss.zzz', dateTime );
  result:= formattedDateTimeStr;
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
  //showmessage('string to covert: ', str);
  glFmtSet:= TFormatSettings.Create();
  glFmtSet.DecimalSeparator := '.';
  glFmtSet.DateSeparator := '.';
  glFmtSet.TimeSeparator := ':';
  glFmtSet.LongTimeFormat := 'hh:nn:ss.zzz';
  glFmtSet.LongDateFormat := 'dd.mm.yyyy';
  glFmtSet.ShortTimeFormat := 'hh:nn:ss.zzz';
  glFmtSet.ShortDateFormat := 'dd.mm.yyyy';

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

// String => TDateTime
// decode of "14.09.2018 10:21:33.784" including msec
function strToTDateTime( str: string ): TDateTime;
begin
  //
end;


// TDateTime => String
// encode of "14.09.2018 10:21:33.784" including msec
function TDateTimeToStr( dateTime:TDateTime ): string;
var
  formattedString: string;
begin
  //
  DateTimeToString( formattedString,    'dd.mm.yyyy hh:nn:ss.zzz', dateTime );
  result:= formattedString;
end;


// TDateTime => String
// encode of "2018 Jun 04 10:21:33.784" including msec
function TDateTimeToNiceStr( dateTime:TDateTime ): string;
var
  formattedString: string;
begin
  //
  DateTimeToString( formattedString,    'yyyy mmm dd hh:nn:ss.zzz', dateTime );
  result:= formattedString;
end;

// TDateTime => String
// decode of "10:21:33.784" including msec
function strTimeToTDateTime( str: string ): TDateTime;
  var
  dateTime:TDateTime;
  glFmtSet: TFormatSettings;
  timestampInt: Int64;
begin

  glFmtSet:= TFormatSettings.Create();
  glFmtSet.DecimalSeparator := '.';
  glFmtSet.TimeSeparator:=':';


  dateTime:= StrToDateTime( str, glFmtSet );
  result:= dateTime;
end;

// TDateTime => String
// encode of "10:21:33.784" including msec
function TDateTimeToTimeStr( dateTime:TDateTime ): string;
var
  formattedString: string;
begin
  //
  DateTimeToString( formattedString,    'hh:nn:ss.zzz', dateTime );
  result:= formattedString;
end;

end.
