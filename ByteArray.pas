unit ByteArray;

interface

uses
  Classes, Types, SysUtils, Math;

procedure ByteArrayToFile( const ByteArray : TByteDynArray; const FileName : string );
function FileToByteArray( const FileName : string ) : TByteDynArray;

implementation

procedure ByteArrayToFile( const ByteArray : TByteDynArray; const FileName : string );
var
  Count : integer;
  F : FIle of Byte;
  pTemp : Pointer;
begin
  AssignFile( F, FileName );
  Rewrite(F);
  try
    Count := Length( ByteArray );
    pTemp := @ByteArray[0];
    BlockWrite(F, pTemp^, Count );
  finally
    CloseFile( F );
  end;
end;

function FileToByteArray( const FileName : string ) : TByteDynArray;
const
  BLOCK_SIZE = 1024;
var
  BytesRead, BytesToWrite, Count : integer;
  F : FIle of Byte;
  pTemp : Pointer;
begin
  AssignFile( F, FileName );
  Reset(F);
  try
    Count := FileSize( F );
    SetLength(Result, Count );
    pTemp := @Result[0];
    BytesRead := BLOCK_SIZE;
    while (BytesRead = BLOCK_SIZE ) do
    begin
      BytesToWrite := Min(Count, BLOCK_SIZE);
      BlockRead(F, pTemp^, BytesToWrite , BytesRead );
      pTemp := Pointer(LongInt(pTemp) + BLOCK_SIZE);
      Count := Count-BytesRead;
    end;
  finally
    CloseFile( F );
  end;
end;

end.
