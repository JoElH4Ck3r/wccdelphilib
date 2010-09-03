unit Territory;

interface

uses
  Classes, SysUtils;

type
  TTerritoryOrder = (toProvFirst, toStateFirst, toAlpha);

  TTerritory = record
    Desc : string[40];
    Code : string[2];
  end;

const
  States : array[0..50] of TTerritory = (
    (Desc : 'Alabama';               Code: 'AL'),  //Alabama AL
    (Desc : 'Alaska';                Code: 'AK'),  //Alaska AK
    (Desc : 'Arizona';               Code: 'AZ'),  //Arizona AZ
    (Desc : 'Arkansas';              Code: 'AR'),  //Arkansas AR
    (Desc : 'California';            Code: 'CA'),  //California CA
    (Desc : 'Colorado';              Code: 'CO'),  //Colorado CO
    (Desc : 'Connecticut';           Code: 'CT'),  //Connecticut CT
    (Desc : 'Delaware';              Code: 'DE'),  //Delaware DE
    (Desc : 'District of Columbia';  Code: 'DC'),  //District of Columbia DC
    (Desc : 'Florida';               Code: 'FL'),  //Florida FL
    (Desc : 'Georgia';               Code: 'GA'),  //Georgia GA
    (Desc : 'Hawaii';                Code: 'HI'),  //Hawaii HI
    (Desc : 'Idaho';                 Code: 'ID'),  //Idaho ID
    (Desc : 'Illinois';              Code: 'IL'),  //Illinois IL
    (Desc : 'Indian';                Code: 'IN'),  //Indiana IN
    (Desc : 'Iowa';                  Code: 'IA'),  //Iowa IA
    (Desc : 'Kansas';                Code: 'KS'),  //Kansas KS
    (Desc : 'Kentucky';              Code: 'KY'),  //Kentucky KY
    (Desc : 'Louisiana';             Code: 'LA'),  //Louisiana LA
    (Desc : 'Main';                  Code: 'ME'),  //Maine ME
    (Desc : 'Maryland';              Code: 'MD'),  //Maryland MD
    (Desc : 'Massachusetts';         Code: 'MA'),  //Massachusetts MA
    (Desc : 'Michigan';              Code: 'MI'),  //Michigan MI
    (Desc : 'Minnesota';             Code: 'MN'),  //Minnesota MN
    (Desc : 'Mississippi';           Code: 'MS'),  //Mississippi MS
    (Desc : 'Missouri';              Code: 'MO'),  //Missouri MO
    (Desc : 'Montana';               Code: 'MT'),  //Montana MT
    (Desc : 'Nebraska';              Code: 'NE'),  //Nebraska NE
    (Desc : 'Nevada';                Code: 'NV'),  //Nevada NV
    (Desc : 'New Hampshire';         Code: 'NH'),  //New Hampshire NH
    (Desc : 'New Jersey';            Code: 'NJ'),  //New Jersey NJ
    (Desc : 'New Mexico';            Code: 'NM'),  //New Mexico NM
    (Desc : 'New York';              Code: 'NY'),  //New York NY
    (Desc : 'North Carolina';        Code: 'NC'),  //North Carolina NC
    (Desc : 'North Dakota';          Code: 'ND'),  //North Dakota ND
    (Desc : 'Ohio';                  Code: 'OH'),  //Ohio OH
    (Desc : 'Oklahoma';              Code: 'OK'),  //Oklahoma OK
    (Desc : 'Oregon';                Code: 'OR'),  //Oregon OR
    (Desc : 'Pennsylvania';          Code: 'PA'),  //Pennsylvania PA
    (Desc : 'Rhode Island';          Code: 'RI'),  //Rhode Island RI
    (Desc : 'South Carolina';        Code: 'SC'),  //South Carolina SC
    (Desc : 'South Dakota';          Code: 'SD'),  //South Dakota SD
    (Desc : 'Tennesse';              Code: 'TN'),  //Tennessee TN
    (Desc : 'Texas';                 Code: 'TX'),  //Texas TX
    (Desc : 'Utah';                  Code: 'UT'),  //Utah UT
    (Desc : 'Vermont';               Code: 'VT'),  //Vermont VT
    (Desc : 'Virginia';              Code: 'VA'),  //Virginia VA
    (Desc : 'Washington';            Code: 'WA'),  //Washington WA
    (Desc : 'West Virginia';         Code: 'WV'),  //West Virginia WV
    (Desc : 'Wisconsin';             Code: 'WI'),  //Wisconsin WI
    (Desc : 'Wyoming';               Code: 'WY')   //Wyoming WY
    );

  Provinces : array[0..12] of TTerritory = (
    (Desc : 'Alberta';               Code: 'AB'),   //Alberta AB
    (Desc : 'British Columbia';      Code: 'BC'),   //British Columbia BC
    (Desc : 'Manitoba';              Code: 'MB'),   //Manitoba MB
    (Desc : 'New Brunswick';         Code: 'NB'),   //New Brunswick NB
    (Desc : 'Newfoundland';          Code: 'NF'),   //Newfoundland NF
    (Desc : 'Northwest Territories'; Code: 'NT'),   //Northwest Territories NT
    (Desc : 'Nova Scotia';           Code: 'NS'),   //Nova Scotia NS
    (Desc : 'Nunavut';               Code: 'NU'),   //Nunavut NU
    (Desc : 'Ontario';               Code: 'ON'),   //Ontario ON
    (Desc : 'Prince Edward Island';  Code: 'PE'),   //Prince Edward Island PE
    (Desc : 'Quebec';                Code: 'QC'),   //Quebec QC
    (Desc : 'Saskatchewan';          Code: 'SK'),   //Saskatchewan SK
    {$IFDEF CSA_ASSISTANT}
    (Desc : 'Yukon';                 Code: 'YK')    //Yukon Territory YK (YT)
    {$ELSE}
    (Desc : 'Yukon';                 Code: 'YT')    //Yukon Territory YK (YT)
    {$ENDIF}
    );

  Countries : array[0..1] of TTerritory = (
    (Desc : 'United States';         Code: 'US'),
    (Desc : 'Canada';                Code: 'CA')
    );

function IsState(const s : string):boolean;
function IsProvince(const s : string):boolean;
function IsTerritory(const s : string):boolean;

function GetCountry(const s : string):string;
function GetCountryDef(const s : string; DefCountry : string):string;
function GetCountryDesc(const s : string):string;
procedure GetStates(AList : TStrings);
function StateToStateCode(const State : string):string;

procedure GetCountries(AList : TStrings);

procedure GetTerritories(AList : TStrings; TerritoryOrder : TTerritoryOrder = toProvFirst);
procedure GetTerritoryCodes(AList : TStrings; TerritoryOrder : TTerritoryOrder = toProvFirst);
function TerritoryDescToCode(const Desc : string):string;
function TerritoryCodeToDesc(const Code : string):string;

implementation

procedure GetCountries(AList : TStrings);
var
  i: Integer;
begin
  if Alist = nil then Exit;
  AList.Clear;
  for i := Low(Countries) to High(Countries) do
    AList.Add(Countries[i].Desc);
end;

procedure GetTerritories(AList : TStrings; TerritoryOrder : TTerritoryOrder = toProvFirst);
var
  i, j : integer;
begin
  if Alist = nil then Exit;
  AList.Clear;
  case TerritoryOrder of
    toProvFirst :
    begin
      for i := Low(Provinces) to High(Provinces) do
        AList.Add(Provinces[i].Desc);
      for j := Low(States) to High(States) do
        AList.Add(States[j].Desc);
    end;
    toStateFirst :
    begin
      for j := Low(States) to High(States) do
        AList.Add(States[j].Desc);
      for i := Low(Provinces) to High(Provinces) do
        AList.Add(Provinces[i].Desc);
    end;
    toAlpha :
    begin
      for i := Low(Provinces) to High(Provinces) do
        AList.Add(Provinces[i].Desc);
      for j := Low(States) to High(States) do
        AList.Add(States[j].Desc);
      { ToDo: Sort the list }
    end;
  end;
end;

procedure GetTerritoryCodes(AList : TStrings; TerritoryOrder : TTerritoryOrder = toProvFirst);
var
  i, j : integer;
begin
  if Alist = nil then Exit;
  AList.Clear;
  case TerritoryOrder of
    toProvFirst :
    begin
      for i := Low(Provinces) to High(Provinces) do
        AList.Add(Provinces[i].Code);
      for j := Low(States) to High(States) do
        AList.Add(States[j].Code);
    end;
    toStateFirst :
    begin
      for j := Low(States) to High(States) do
        AList.Add(States[j].Code);
      for i := Low(Provinces) to High(Provinces) do
        AList.Add(Provinces[i].Code);
    end;
    toAlpha :
    begin
      for i := Low(Provinces) to High(Provinces) do
        AList.Add(Provinces[i].Code);
      for j := Low(States) to High(States) do
        AList.Add(States[j].Code);
      { ToDo: Sort the list }
    end;
  end;
end;

function TerritoryDescToCode(const Desc : string):string;
var
  i, j : integer;
begin
  Result := '';
  for i := Low(Provinces) to High(Provinces) do
    if Provinces[i].Desc = Desc then begin
      Result := Provinces[i].Code;
      Exit;
    end;
  for i := Low(States) to High(States) do
    if States[i].Desc = Desc then begin
      Result := States[i].Code;
      Exit;
    end;
end;

function TerritoryCodeToDesc(const Code : string):string;
var
  i, j : integer;
begin
  Result := '';
  for i := Low(Provinces) to High(Provinces) do
    if Provinces[i].Code = Code then begin
      Result := Provinces[i].Desc;
      Exit;
    end;
  for i := Low(States) to High(States) do
    if States[i].Code = Code then begin
      Result := States[i].Desc;
      Exit;
    end;
end;

function StateToStateCode(const State : string):string;
var
  i : integer;
begin
  for i := Low(States) to High(States) do
    if State = States[i].Desc then begin
      Result := States[i].Code;
      Exit;
    end;
end;

procedure GetStates(AList : TStrings);
var
  i : integer;
begin
  if AList = nil then Exit;
  AList.Clear;
  for i := Low(States) to High(States) do
    AList.Add(States[i].Desc);
end;

function IsState(const s : string):boolean;
var
  i : integer;
begin
  Result := False;
  for i := Low(States) to High (States) do
    if CompareText(States[i].Code, Trim(s)) = 0 then begin
      Result := True;
      Exit
    end;
end;

function IsProvince(const s : string):boolean;
var
  i : integer;
begin
  Result := False;
  for i := Low(Provinces) to High (Provinces) do
    if CompareText(Provinces[i].Code, Trim(s)) = 0 then begin
      Result := True;
      Exit
    end;
end;

function IsTerritory(const s : string):boolean;
begin
  Result := IsProvince(s) or IsState(s);
end;

function GetCountry(const s : string):string;
begin
  Result := 'US';
  if IsProvince(s) then
    Result := 'CA';
end;

function GetCountryDef(const s : string; DefCountry : string):string;
begin
  Result := DefCountry;
  if IsProvince(s) then
    Result := 'CA';
end;

function GetCountryDesc(const s : string):string;
begin
  Result := Countries[0].Desc;
  if IsProvince(s) then
    Result := Countries[1].Desc;
end;

end.
