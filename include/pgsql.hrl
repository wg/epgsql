-record(column,    {name, type, size, modifier, format}).
-record(statement, {name, columns, types}).

-record(error,  {severity, type, code, message, extra}).
