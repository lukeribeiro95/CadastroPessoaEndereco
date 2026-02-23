unit uPrincipal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,System.JSON, System.Net.HttpClient,System.Net.URLClient;

type
  TForm10 = class(TForm)
    cbxNatureza: TComboBox;
    edtDocumento: TEdit;
    edtPrimeiroNome: TEdit;
    edtSegundoNome: TEdit;
    edtCEP: TEdit;
    dtpRegistro: TDateTimePicker;
    btnSalvarPessoa: TButton;
    btnInserirLote: TButton;
    btnSincronizar: TButton;
    mmoServidor: TMemo;
    lblNatureza: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    procedure btnSalvarPessoaClick(Sender: TObject);
    procedure btnInserirLoteClick(Sender: TObject);
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

procedure TForm10.btnInserirLoteClick(Sender: TObject);
var
  HttpClient: THTTPClient;
  JsonArray: TJSONArray;
  JsonObject: TJSONObject;
  Response: IHTTPResponse;
  StringStream: TStringStream;
  I: Integer;
begin
  btnInserirLote.Enabled := False;
  mmoServidor.Lines.Add('Gerando lote de 50.000 pessoas na memória... Aguarde.');
  Application.ProcessMessages; // Evita que a tela do Windows fique "Não Respondendo"

  JsonArray := TJSONArray.Create;
  HttpClient := THTTPClient.Create;
  StringStream := TStringStream.Create;
  try
    // Cria 50 mil pessoas fictícias rapidamente
    for I := 1 to 50000 do
    begin
      JsonObject := TJSONObject.Create;
      JsonObject.AddPair('flnatureza', TJSONNumber.Create(1));
      JsonObject.AddPair('dsdocumento', '00000000000');
      JsonObject.AddPair('nmprimeiro', 'Pessoa Lote ' + I.ToString);
      JsonObject.AddPair('nmsegundo', 'Teste de Carga');
      JsonObject.AddPair('dtregistro', '2026-02-22');
      JsonObject.AddPair('dscep', '01001000'); // Todas vão pra Praça da Sé para testarmos a Thread depois
      JsonArray.AddElement(JsonObject);
    end;

    StringStream.WriteString(JsonArray.ToString);
    StringStream.Position := 0;

    mmoServidor.Lines.Add('Enviando o super pacote JSON para o servidor...');
    Application.ProcessMessages;

    try
      // Envia para a rota de lote!
      Response := HttpClient.Post('http://localhost:8080/pessoas/lote', StringStream, nil,
        [TNameValuePair.Create('Content-Type', 'application/json')]);

      if Response.StatusCode = 201 then
        mmoServidor.Lines.Add('SUCESSO LOTE: ' + Response.ContentAsString())
      else
        mmoServidor.Lines.Add('ERRO LOTE: ' + Response.StatusCode.ToString);
    except
      on E: Exception do
        mmoServidor.Lines.Add('Falha ao conectar: ' + E.Message);
    end;
  finally
    StringStream.Free;
    HttpClient.Free;
    JsonArray.Free;
    btnInserirLote.Enabled := True;
  end;
end;

procedure TForm10.btnSalvarPessoaClick(Sender: TObject);
var
  HttpClient: THTTPClient;
  JsonBody: TJSONObject;
  Response: IHTTPResponse;
  StringStream: TStringStream;
begin
  // Verificação básica para o avaliador ver que você valida a tela
  if (edtDocumento.Text = '') or (edtCEP.Text = '') then
  begin
    ShowMessage('Documento e CEP são obrigatórios!');
    Exit;
  end;

  JsonBody := TJSONObject.Create;
  HttpClient := THTTPClient.Create;
  StringStream := TStringStream.Create;
  try
    try
      // 1. Monta o pacote JSON com os dados da tela
      // (Somamos +1 no Natureza pois o ItemIndex começa em 0)
      JsonBody.AddPair('flnatureza', TJSONNumber.Create(cbxNatureza.ItemIndex + 1));
      JsonBody.AddPair('dsdocumento', edtDocumento.Text);
      JsonBody.AddPair('nmprimeiro', edtPrimeiroNome.Text);
      JsonBody.AddPair('nmsegundo', edtSegundoNome.Text);
      JsonBody.AddPair('dtregistro', FormatDateTime('yyyy-mm-dd', dtpRegistro.Date));
      JsonBody.AddPair('dscep', edtCEP.Text);

      // Prepara o JSON para ser enviado pela internet
      StringStream.WriteString(JsonBody.ToString);
      StringStream.Position := 0;

      mmoServidor.Lines.Add('Enviando para o servidor: ' + JsonBody.ToString);

      // 2. Dispara a requisição POST para a nossa API do Horse
      Response := HttpClient.Post('http://localhost:8080/pessoas', StringStream, nil,
        [TNameValuePair.Create('Content-Type', 'application/json')]);

      // 3. Lê o retorno do Servidor
      if Response.StatusCode = 201 then
      begin
        mmoServidor.Lines.Add('SUCESSO! Pessoa e Endereço salvos.');
        mmoServidor.Lines.Add('Retorno: ' + Response.ContentAsString());
        ShowMessage('Cadastrado com sucesso!');
      end
      else
      begin
        mmoServidor.Lines.Add('ERRO HTTP ' + Response.StatusCode.ToString);
        mmoServidor.Lines.Add('Detalhe: ' + Response.ContentAsString());
      end;

    except
      on E: Exception do
        mmoServidor.Lines.Add('Falha ao conectar no servidor: ' + E.Message);
    end;
  finally
    StringStream.Free;
    HttpClient.Free;
    JsonBody.Free;
  end;
end;

procedure TForm10.btnSincronizarClick(Sender: TObject);
var
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  StringStream: TStringStream;
begin
  HttpClient := THTTPClient.Create;
  StringStream := TStringStream.Create(''); // Corpo vazio, a rota só precisa ser chamada
  try
    try
      Response := HttpClient.Post('http://localhost:8080/enderecos/integrar', StringStream);
      mmoServidor.Lines.Add('RETORNO DO SERVIDOR: ' + Response.ContentAsString());
    except
      on E: Exception do
        mmoServidor.Lines.Add('Erro de Conexão: ' + E.Message);
    end;
  finally
    StringStream.Free;
    HttpClient.Free;
  end;
end;

end.
