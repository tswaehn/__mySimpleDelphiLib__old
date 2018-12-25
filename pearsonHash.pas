unit pearsonHash;

interface
uses dialogs;

type
    TByteArray = array of byte;

  (*
      implementation of PEARSON HASH
      https://en.wikipedia.org/wiki/Pearson_hashing

      is known to be "Perfect hash function"
      https://en.wikipedia.org/wiki/Perfect_hash_function

  *)
  function pearsonHash24( byteArray:array of byte; len:integer ):integer;
  function pearsonHash24fromInt( int:int64 ):integer;

implementation

Type
TInt64Rec = packed record
  case Integer of
    1 : (_Byte : Array[0..7] of Byte);
    2 : (_Int64 : Int64);

end;

TInt32Rec = packed record
  case Integer of
    1 : (_Byte : Array[0..3] of Byte);
    2 : (_Int32 : Int32);

end;

const
    T: array[0..255] of integer = (

      // 0-255 shuffled in any (random) order suffices

       98,  6, 85,150, 36, 23,112,164,135,207,169,  5, 26, 64,165,219, //  1
       61, 20, 68, 89,130, 63, 52,102, 24,229,132,245, 80,216,195,115, //  2
       90,168,156,203,177,120,  2,190,188,  7,100,185,174,243,162, 10, //  3
      237, 18,253,225,  8,208,172,244,255,126,101, 79,145,235,228,121, //  4
      123,251, 67,250,161,  0,107, 97,241,111,181, 82,249, 33, 69, 55, //  5
       59,153, 29,  9,213,167, 84, 93, 30, 46, 94, 75,151,114, 73,222, //  6
      197, 96,210, 45, 16,227,248,202, 51,152,252,125, 81,206,215,186, //  7
       39,158,178,187,131,136,  1, 49, 50, 17,141, 91, 47,129, 60, 99, //  8
      154, 35, 86,171,105, 34, 38,200,147, 58, 77,118,173,246, 76,254, //  9
      133,232,196,144,198,124, 53,  4,108, 74,223,234,134,230,157,139, // 10
      189,205,199,128,176, 19,211,236,127,192,231, 70,233, 88,146, 44, // 11
      183,201, 22, 83, 13,214,116,109,159, 32, 95,226,140,220, 57, 12, // 12
      221, 31,209,182,143, 92,149,184,148, 62,113, 65, 37, 27,106,166, // 13
        3, 14,204, 72, 21, 41, 56, 66, 28,193, 40,217, 25, 54,179,117, // 14
      238, 87,240,155,180,170,242,212,191,163, 78,218,137,194,175,110, // 15
       43,119,224, 71,122,142, 42,160,104, 48,247,103, 15, 11,138,239  // 16
     );

// generate a 24bit hash
function pearsonHash24( byteArray: array of byte; len:integer ):integer;
var
  j: Integer;
  h:byte;
  b:byte;
  i: Integer;
  res: TInt32Rec;
begin

  // reset
  res._Int32:= 0;

  // create only 3bytes (not 4bytes) => 24bit
  for j := 0 to 2 do begin
    b:= byteArray[0];
    h:= T[ (b + j) mod 256 ];
    for i := 1 to len do begin
      b:= byteArray[i];
      h:= T[ h xor b ];
    end;
    res._Byte[j]:= h;
  end;

  i:= res._Int32;
  if (i<0) or (i> 16777215) then begin
    showmessage('invalid hash24');
  end;

  // attention: lsb is bit31, msb is bit0, and dont touch the bit31 => this is sign bit :)
  result:= res._Int32;
end;

function pearsonHash24fromInt( int:int64 ):integer;
var byteArray: TInt64Rec;
begin
  byteArray._Int64:= int;
  result:= pearsonHash24( byteArray._Byte, 8 );
end;

end.
