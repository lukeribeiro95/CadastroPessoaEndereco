unit uEnderecoDAO;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Net.HttpClient,
  FireDAC.Comp.Client, uEndereco, uConexao;

type
  TEnderecoDAO = class
  private
    FConexao: TdmConexao;
  public
    constructor Create;
    destructor Destroy; override;

    function Insert(AEndereco: TEndereco): Boolean;
    procedure ProcessarIntegracoesPendentes;
  end;

implementation

{ TEnderecoDAO }

constructor TEnderecoDAO.Create;
begin
  FConexao := TdmConexao.Create(nil);
end;

destructor TEnderecoDAO.Destroy;
begin
  FConexao.Free;
  inherited;
end;

function TEnderecoDAO.Insert(AEndereco: TEndereco): Boolean;
var
  vltQry: TFDQuery;
  vlsThreadCep: string;
  vliThreadIdEndereco: Int64;
begin
  vltQry := TFDQuery.Create(nil);
  try
    vltQry.Connection := FConexao.FDConnection1;
    //insere na tabela de endereço
    vltQry.SQL.Add('INSERT INTO endereco (idpessoa, dscep)');
    vltQry.SQL.Add('VALUES (:pessoa, :cep) RETURNING idendereco');

    vltQry.ParamByName('pessoa').AsLargeInt := AEndereco.IdPessoa;
    vltQry.ParamByName('cep').AsString := AEndereco.DsCep;
    vltQry.Open;

    // pega o id
    AEndereco.IdEndereco := vltQry.FieldByName('idendereco').AsLargeInt;

    // prepara para chamar o thread
    vlsThreadCep := AEndereco.DsCep;
    vliThreadIdEndereco := AEndereco.IdEndereco;

    // inicia o thread
    TThread.CreateAnonymousThread(
      procedure
      var
        vltHttp: THTTPClient;
        vltResponse: IHTTPResponse;
        vltJsonObj: TJSONObject;
        vltQryIntegracao: TFDQuery;
        vltConexaoThread: TdmConexao; // cria uma nova conexão apenas para o thread
      begin
        vltHttp := THTTPClient.Create;
        vltConexaoThread := TdmConexao.Create(nil);
        vltQryIntegracao := TFDQuery.Create(nil);
        try
          try
            // Busca no ViaCEP
            vltResponse := vltHttp.Get('https://viacep.com.br/ws/' + vlsThreadCep + '/json/');

            if vltResponse.StatusCode = 200 then
            begin
              vltJsonObj := TJSONObject.ParseJSONValue(vltResponse.ContentAsString()) as TJSONObject;
              if Assigned(vltJsonObj) then
              try
                // Se o ViaCEP não retornou "erro": true
                if vltJsonObj.GetValue('erro') = nil then
                begin
                  vltQryIntegracao.Connection := vltConexaoThread.FDConnection1;
                  vltQryIntegracao.SQL.Add('INSERT INTO endereco_integracao (idendereco, dsuf, nmcidade, nmbairro, nmlogradouro, dscomplemento)');
                  vltQryIntegracao.SQL.Add('VALUES (:id, :uf, :cidade, :bairro, :logradouro, :complemento)');

                  vltQryIntegracao.ParamByName('id').AsLargeInt := vliThreadIdEndereco;
                  vltQryIntegracao.ParamByName('uf').AsString := vltJsonObj.GetValue('uf').Value;
                  vltQryIntegracao.ParamByName('cidade').AsString := vltJsonObj.GetValue('localidade').Value;
                  vltQryIntegracao.ParamByName('bairro').AsString := vltJsonObj.GetValue('bairro').Value;
                  vltQryIntegracao.ParamByName('logradouro').AsString := vltJsonObj.GetValue('logradouro').Value;

                  // Complemento pode vir vazio do ViaCEP
                  if vltJsonObj.GetValue('complemento') <> nil then
                    vltQryIntegracao.ParamByName('complemento').AsString := vltJsonObj.GetValue('complemento').Value
                  else
                    vltQryIntegracao.ParamByName('complemento').AsString := '';

                  vltQryIntegracao.ExecSQL;
                end;
              finally
                vltJsonObj.Free;
              end;
            end;
          except
            // except vazio para estourar erro ao usuario caso a conexão caia ou algum outro erro
          end;
        finally
          vltQryIntegracao.Free;
          vltConexaoThread.Free;
          vltHttp.Free;
        end;
      end
    ).Start; // start do thread

    Result := True; // retorna true sem esperar o thread acabar
  finally
    vltQry.Free;
  end;
end;

procedure TEnderecoDAO.ProcessarIntegracoesPendentes;
var
  vltQryBusca: TFDQuery;
begin
  vltQryBusca := TFDQuery.Create(nil);
  try
    vltQryBusca.Connection := FConexao.FDConnection1;
    // Busca todos os endereços que AINDA NÃO estão na tabela endereco_integracao
    vltQryBusca.SQL.Add('SELECT e.idendereco, e.dscep FROM endereco e');
    vltQryBusca.SQL.Add('LEFT JOIN endereco_integracao ei ON e.idendereco = ei.idendereco');
    vltQryBusca.SQL.Add('WHERE ei.idendereco IS NULL');
    vltQryBusca.Open;

    // Para cada registro encontrado, dispara uma Thread de integração
    while not vltQryBusca.Eof do
    begin
      var ThreadIdEndereco := vltQryBusca.FieldByName('idendereco').AsLargeInt;
      var ThreadCep := vltQryBusca.FieldByName('dscep').AsString;

      TThread.CreateAnonymousThread(
        procedure
        var
          vltHttp: THTTPClient;
          vltResponse: IHTTPResponse;
          vltJsonObj: TJSONObject;
          vltQryInt: TFDQuery;
          vltConThread: TdmConexao;  // cria uma nova conexão apenas para o thread
        begin
          vltHttp := THTTPClient.Create;
          vltConThread := TdmConexao.Create(nil);
          vltQryInt := TFDQuery.Create(nil);
          try
            try
              vltResponse := vltHttp.Get('https://viacep.com.br/ws/' + ThreadCep + '/json/');
              if vltResponse.StatusCode = 200 then
              begin
                vltJsonObj := TJSONObject.ParseJSONValue(vltResponse.ContentAsString()) as TJSONObject;
                if Assigned(vltJsonObj) then
                try
                  if vltJsonObj.GetValue('erro') = nil then
                  begin
                    vltQryInt.Connection := vltConThread.FDConnection1;
                    vltQryInt.SQL.Add('INSERT INTO endereco_integracao (idendereco, dsuf, nmcidade, nmbairro, nmlogradouro, dscomplemento)');
                    vltQryInt.SQL.Add('VALUES (:id, :uf, :cidade, :bairro, :logradouro, :complemento)');

                    vltQryInt.ParamByName('id').AsLargeInt := ThreadIdEndereco;
                    vltQryInt.ParamByName('uf').AsString := vltJsonObj.GetValue('uf').Value;
                    vltQryInt.ParamByName('cidade').AsString := vltJsonObj.GetValue('localidade').Value;
                    vltQryInt.ParamByName('bairro').AsString := vltJsonObj.GetValue('bairro').Value;
                    vltQryInt.ParamByName('logradouro').AsString := vltJsonObj.GetValue('logradouro').Value;

                    if vltJsonObj.GetValue('complemento') <> nil then
                      vltQryInt.ParamByName('complemento').AsString := vltJsonObj.GetValue('complemento').Value
                    else
                      vltQryInt.ParamByName('complemento').AsString := '';

                    vltQryInt.ExecSQL;
                  end;
                finally
                  vltJsonObj.Free;
                end;
              end;
            except
              // except vazio para a thread não travar o loop
            end;
          finally
            vltQryInt.Free;
            vltConThread.Free;
            vltHttp.Free;
          end;
        end
      ).Start; // Inicia a Thread para este CEP específico

      vltQryBusca.Next;
    end;
  finally
    vltQryBusca.Free;
  end;
end;

end.
