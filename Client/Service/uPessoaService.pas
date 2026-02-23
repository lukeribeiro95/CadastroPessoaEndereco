unit uPessoaService;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Net.HttpClient, System.Net.URLClient;

type
  TPessoaService = class
  public
    function SalvarPessoa(ANatureza: Integer; const ADocumento, APrimeiroNome, ASegundoNome,
      ADataRegistro, ACep: string; out ARetorno: string): Boolean;
    function SincronizarEnderecos(out ARetorno: string): Boolean;
  end;

implementation

const
  BASE_URL = 'http://localhost:8080';

{ TPessoaService }

function TPessoaService.SalvarPessoa(ANatureza: Integer; const ADocumento, APrimeiroNome,
  ASegundoNome, ADataRegistro, ACep: string; out ARetorno: string): Boolean;
var
  vltHttpClient: THTTPClient;
  vltJsonBody: TJSONObject;
  vltResponse: IHTTPResponse;
  vltStringStream: TStringStream;
begin
  Result := False;

  vltJsonBody := nil;
  vltHttpClient := nil;
  vltStringStream := nil;

  try
    try
      vltJsonBody := TJSONObject.Create;
      vltHttpClient := THTTPClient.Create;

      // Monta o JSON com os parâmetros recebidos
      vltJsonBody.AddPair('flnatureza', TJSONNumber.Create(ANatureza));
      vltJsonBody.AddPair('dsdocumento', ADocumento);
      vltJsonBody.AddPair('nmprimeiro', APrimeiroNome);
      vltJsonBody.AddPair('nmsegundo', ASegundoNome);
      vltJsonBody.AddPair('dtregistro', ADataRegistro);
      vltJsonBody.AddPair('dscep', ACep);

      vltStringStream := TStringStream.Create(vltJsonBody.ToString);
      vltStringStream.Position := 0;

      vltResponse := vltHttpClient.Post(BASE_URL + '/pessoas', vltStringStream, nil,
        [TNameValuePair.Create('Content-Type', 'application/json')]);

      ARetorno := vltResponse.ContentAsString();

      if vltResponse.StatusCode = 201 then
        Result := True
      else
        ARetorno := 'ERRO HTTP ' + vltResponse.StatusCode.ToString + ' - ' + ARetorno;

    except
      on vltE: Exception do
        ARetorno := 'Falha de conexão: ' + vltE.Message;
    end;
  finally
    vltStringStream.Free;
    vltHttpClient.Free;
    vltJsonBody.Free;
  end;
end;

function TPessoaService.SincronizarEnderecos(out ARetorno: string): Boolean;
var
  vltHttpClient: THTTPClient;
  vltResponse: IHTTPResponse;
  vltStringStream: TStringStream;
begin
  Result := False;
  vltHttpClient := nil;
  vltStringStream := nil;

  try
    try
      vltHttpClient := THTTPClient.Create;
      vltStringStream := TStringStream.Create('');

      vltResponse := vltHttpClient.Post(BASE_URL + '/enderecos/integrar', vltStringStream);
      ARetorno := vltResponse.ContentAsString();

      Result := (vltResponse.StatusCode = 202) or (vltResponse.StatusCode = 200);
    except
      on vltE: Exception do
        ARetorno := 'Falha de conexão: ' + vltE.Message;
    end;
  finally
    vltStringStream.Free;
    vltHttpClient.Free;
  end;
end;

end.
