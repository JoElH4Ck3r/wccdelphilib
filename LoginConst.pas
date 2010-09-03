unit LoginConst;

interface

const
  ConStr = 'Provider=MSDASQL.1;Password=%s;Persist Security Info=True;User ID=%s;Data Source=%s';
  ConStrWin = 'Provider=MSDASQL.1;Persist Security Info=False;Data Source=%s';
  RegLastUser = 'LastUser';
  RegLastDB = 'LastDB';
  RegLastSchema = 'LastSchema';
  RegLastPath = 'LastPath';

  DefaultMaxAttempts = 5;
  DefaultRegKey = 'Connection';

  ConnectionMaxDropdownCount = 20;
  PasswordCharacter = '*';

  ERR_MSG_INVALID_DSN_LIST = 'Invalid DSN List';


implementation

end.
