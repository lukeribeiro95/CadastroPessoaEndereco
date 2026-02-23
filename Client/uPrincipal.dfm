object Form10: TForm10
  Left = 0
  Top = 0
  Caption = 'Form10'
  ClientHeight = 414
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  TextHeight = 13
  object lblNatureza: TLabel
    Left = 64
    Top = 11
    Width = 44
    Height = 13
    Caption = 'Natureza'
  end
  object Label1: TLabel
    Left = 64
    Top = 37
    Width = 54
    Height = 13
    Caption = 'Documento'
  end
  object Label2: TLabel
    Left = 64
    Top = 64
    Width = 68
    Height = 13
    Caption = 'Primeiro Nome'
  end
  object Label3: TLabel
    Left = 64
    Top = 91
    Width = 72
    Height = 13
    Caption = 'Segundo Nome'
  end
  object Label4: TLabel
    Left = 64
    Top = 118
    Width = 19
    Height = 13
    Caption = 'CEP'
  end
  object Label5: TLabel
    Left = 64
    Top = 146
    Width = 66
    Height = 13
    Caption = 'Data Registro'
  end
  object cbxNatureza: TComboBox
    Left = 144
    Top = 8
    Width = 145
    Height = 21
    ItemIndex = 0
    TabOrder = 0
    Text = 'Fisica'
    Items.Strings = (
      'Fisica'
      'Juridica')
  end
  object edtDocumento: TEdit
    Left = 144
    Top = 34
    Width = 121
    Height = 21
    NumbersOnly = True
    TabOrder = 1
  end
  object edtPrimeiroNome: TEdit
    Left = 144
    Top = 61
    Width = 121
    Height = 21
    TabOrder = 2
  end
  object edtSegundoNome: TEdit
    Left = 144
    Top = 88
    Width = 121
    Height = 21
    TabOrder = 3
  end
  object edtCEP: TEdit
    Left = 144
    Top = 115
    Width = 121
    Height = 21
    NumbersOnly = True
    TabOrder = 4
  end
  object dtpRegistro: TDateTimePicker
    Left = 144
    Top = 142
    Width = 186
    Height = 21
    Date = 46075.000000000000000000
    Time = 0.750519687499036100
    TabOrder = 5
  end
  object btnSalvarPessoa: TButton
    Left = 474
    Top = 32
    Width = 91
    Height = 25
    Caption = 'Salvar Pessoa'
    TabOrder = 6
    OnClick = btnSalvarPessoaClick
  end
  object btnInserirLote: TButton
    Left = 474
    Top = 63
    Width = 113
    Height = 25
    Caption = 'Inserir Lote(50.000)'
    TabOrder = 7
    OnClick = btnInserirLoteClick
  end
  object btnSincronizar: TButton
    Left = 474
    Top = 94
    Width = 129
    Height = 25
    Caption = 'Sincronizar CEPs(Thread)'
    TabOrder = 8
    OnClick = btnSincronizarClick
  end
  object mmoServidor: TMemo
    Left = 8
    Top = 185
    Width = 609
    Height = 184
    ReadOnly = True
    TabOrder = 9
  end
end
