unit uILogger;

interface

type
  ILogger = Interface(IInterface)
    procedure LogInfo(Text: String; StreamIndex: Smallint = -1;
      StreamToken: String = '');
    procedure LogError(Text: String; StreamIndex: Smallint = -1;
      StreamToken: String = '');
    procedure LogWarning(Text: String; StreamIndex: Smallint = -1;
      StreamToken: String = '');
  End;

implementation

end.
