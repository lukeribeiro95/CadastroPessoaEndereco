unit uPrincipal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, uPessoaService;

type
  TForm10 = class(TForm)
    cbxNatureza: TComboBox;
    edtDocumento: TEdit;
    edtPrimeiroNome: TEdit;
    edtSegundoNome: TEdit;
    edtCEP: TEdit;
    dtpRegistro: TDateTimePicker;
    btnSalvarPessoa: TButton;
    btnSincronizar: TButton;
    mmoServidor: TMemo;
    lblNatureza: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    procedure btnSalvarPessoaClick(Sender: TObject);
    procedure btnSincronizarClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form10: TForm10;

implementation

{$R *.dfm}

procedure TForm10.btnSalvarPessoaClick(Sender: TObject);
var
  vltPessoaService: TPessoaService;
  vlsMensagemRetorno: string;
begin
  if (edtDocumento.Text = '') or (edtCEP.Text = '') then
  begin
    ShowMessage('Documento e CEP são obrigatórios!');
    Exit;
  end;

  btnSalvarPessoa.Enabled := False;
  btnSincronizar.Enabled := False;
  mmoServidor.Lines.Add('Processando...');
  Application.ProcessMessages;

  vltPessoaService := nil;
  try
    try
      vltPessoaService := TPessoaService.Create;

      if vltPessoaService.SalvarPessoa(
           cbxNatureza.ItemIndex + 1,
           edtDocumento.Text,
           edtPrimeiroNome.Text,
           edtSegundoNome.Text,
           FormatDateTime('yyyy-mm-dd', dtpRegistro.Date),
           edtCEP.Text,
           vlsMensagemRetorno
         ) then
      begin
        mmoServidor.Lines.Add('SUCESSO: ' + vlsMensagemRetorno);
        ShowMessage('Cadastrado com sucesso!');

        edtDocumento.Clear;
        edtPrimeiroNome.Clear;
        edtSegundoNome.Clear;
        edtCEP.Clear;
        cbxNatureza.ItemIndex := 0;
        edtDocumento.SetFocus;
      end
      else
      begin
        mmoServidor.Lines.Add('ERRO: ' + vlsMensagemRetorno);
      end;
    except
      on vltE: Exception do
        mmoServidor.Lines.Add('Falha crítica na tela: ' + vltE.Message);
    end;
  finally
    vltPessoaService.Free;

    btnSalvarPessoa.Enabled := True;
    btnSincronizar.Enabled := True;
  end;
end;

procedure TForm10.btnSincronizarClick(Sender: TObject);
var
  vltPessoaService: TPessoaService;
  vlsMensagemRetorno: string;
begin
  btnSincronizar.Enabled := False;
  btnSalvarPessoa.Enabled := False;
  mmoServidor.Lines.Add('Aguardando sincronização...');
  Application.ProcessMessages;

  vltPessoaService := nil;
  try
    try
      vltPessoaService := TPessoaService.Create;

      if vltPessoaService.SincronizarEnderecos(vlsMensagemRetorno) then
        mmoServidor.Lines.Add('SINCRONIZAÇÃO INICIADA: ' + vlsMensagemRetorno)
      else
        mmoServidor.Lines.Add('ERRO NA SINCRONIZAÇÃO: ' + vlsMensagemRetorno);
    except
      on vltE: Exception do
        mmoServidor.Lines.Add('Falha crítica na tela: ' + vltE.Message);
    end;
  finally
    vltPessoaService.Free;

    btnSincronizar.Enabled := True;
    btnSalvarPessoa.Enabled := True;
  end;
end;

end.
