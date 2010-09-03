unit VersionInfo;

interface

uses Classes, Windows, sysutils;

function GetFileVersion(filename: string):string;
function GetFileCopyright(filename: string):string;
function GetFileProductName(filename: string):string;
function GetFileProductVersion(filename: string):string;

implementation

type
  TVersionType = (vtFileVersion, vtCopyright, vtProductName, vtProductVersion);

type
  PTransBuffer = ^TTransBuffer;
  TTransBuffer = array[1..4] of smallint;

const
  CStringFileInfo = 'StringFileInfo\';
  CTranslation    = '\VarFileInfo\Translation';

const
  CInfoStr : array[0..3] of string =
    ('FileVersion','LegalCopyright', 'ProductName', 'ProductVersion');

function GetVersionInfo(const FileName : string; VersionItem : TVersionType):string;
var
//  i:        integer;
  infoSize: DWORD;
  ptrans:   PTransBuffer;
  transStr: string;
  typeStr:  string;
  value:    PChar;
  verBuf:   pointer;
  verSize:  DWORD;
  wnd:      DWORD;
begin
  Result := '';
  infoSize := GetFileVersioninfoSize(PChar(FileName), wnd);
  if infoSize <> 0 then
  begin
    GetMem(verBuf, infoSize);
    try
      if GetFileVersionInfo(PChar(FileName), wnd, infoSize, verBuf) then begin
        VerQueryvalue(verBuf, PChar(CTranslation),Pointer(ptrans), verSize);
        transStr := IntToHex(ptrans^[1], 4) + IntToHex(ptrans^[2], 4);
        typeStr := CStringFileInfo + transStr + '\' + CInfoStr[integer(VersionItem)];
        if VerQueryvalue(verBuf, PChar(typeStr),Pointer(value), verSize) then
          Result := value;
      end;
    finally
      FreeMem(verBuf);
    end;
  end
end;

function GetFileVersion(filename: string):string;
begin
  Result := GetVersionInfo(FileName, vtFileVersion);
end;

function GetFileCopyright(filename: string):string;
begin
  Result := GetVersionInfo(FileName, vtCopyright);
end;

function GetFileProductName(filename: string):string;
begin
  Result := GetVersionInfo(FileName, vtProductName);
end;

function GetFileProductVersion(filename: string):string;
begin
  Result := GetVersionInfo(FileName, vtProductVersion);
end;

end.
