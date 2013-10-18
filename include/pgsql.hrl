-record(column,    {name, type, size, modifier, format}).
-record(statement, {name, columns, types}).

-record(error,  {severity, code, codename, message, extra}).
