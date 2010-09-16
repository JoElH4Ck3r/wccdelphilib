unit Currency;

interface

uses
  Classes, SysUtils;

type
  TCurrency = record
    Code : string[3];
    Desc : string[40];
  end;

const
  CUR_CAD = 'CAD';
  CUR_USD = 'USD';

  Currencies : array[0..1] of TCurrency = (
    (Code: CUR_CAD; Desc: 'Canadian Dollar'),
    (Code: CUR_USD; Desc: 'United States Dollars')
//    (Code: 'EUR'; Desc: 'Euro'),
//    (Code: 'GBP'; Desc: 'United Kingdom Pounds'),
//    (Code: 'AUD'; Desc: 'Austrailian Dollars')
    );


procedure GetCurrencies(AList : TStrings);

function CurrencyDescToCode(const Desc : string):string;
function CurrencyCodeToDesc(const Code : string):string;

implementation

procedure GetCurrencies(AList : TStrings);
var
  i : integer;
begin
  if Alist = nil then Exit;
  AList.Clear;
  for i := Low(Currencies) to High(Currencies) do
    AList.Add(Currencies[i].Desc);
end;

function CurrencyDescToCode(const Desc : string):string;
var
  i : integer;
begin
  Result := '';
  for i := Low(Currencies) to High(Currencies) do
    if Currencies[i].Desc = Desc then begin
      Result := Currencies[i].Code;
      Exit;
    end;
end;

function CurrencyCodeToDesc(const Code : string):string;
var
  i : integer;
begin
  Result := '';
  for i := Low(Currencies) to High(Currencies) do
    if Currencies[i].Code = Code then begin
      Result := Currencies[i].Desc;
      Exit;
    end;
end;

end.
