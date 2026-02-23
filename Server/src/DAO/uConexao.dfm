object dmConexao: TdmConexao
  Height = 480
  Width = 640
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Database=Teste'
      'User_Name=postgres'
      'DriverID=PG')
    LoginPrompt = False
    Left = 120
    Top = 88
  end
  object FDPhysPgDriverLink1: TFDPhysPgDriverLink
    VendorLib = 'C:\TesteDelphi\Dlls\libpq.dll'
    Left = 240
    Top = 88
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 368
    Top = 88
  end
end
