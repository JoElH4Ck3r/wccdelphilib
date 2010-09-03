unit DBSearchDlgThread;

interface

uses
  Classes, ADODB, ADOINT, Sysutils, DBConnection, SQLTypes, DB, ActiveX, Windows;

const
  MAX_SORT_COL = 5;

type
  TThreadError = procedure(const ErrorMessage : string) of object;
  TThreadMessage = procedure(const Msg : string) of object;
  
implementation

end.
