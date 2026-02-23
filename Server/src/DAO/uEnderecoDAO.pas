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
    function Update(AEndereco: TEndereco): Boolean;
    function Delete(AIdEndereco: Int64): Boolean;
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
  vltQry := nil;
  try
    vltQry := TFDQuery.Create(nil);
    vltQry.Connection := FConexao.FDConnection1;

    // insere na tabela de endereço
    vltQry.SQL.Add('INSERT INTO endereco (idpessoa, dscep)');
    vltQry.SQL.Add('VALUES (:pessoa, :cep) RETURNING idendereco');

    vltQry.ParamByName('pessoa').AsLargeInt := AEndereco.IdPessoa;
    vltQry.ParamByName('cep').AsString := AEndereco.DsCep;
    vltQry.Open;

    // pega o id
    AEndereco.IdEndereco := vltQry.FieldByName('idendereco').AsLargeInt;

    vlsThreadCep := AEndereco.DsCep;
    vliThreadIdEndereco := AEndereco.IdEndereco;

    // inicia a thread
    TThread.CreateAnonymousThread(
      procedure
      var
        vltHttp: THTTPClient;
        vltResponse: IHTTPResponse;
        vltJsonObj: TJSONObject;
        vltQryIntegracao: TFDQuery;
        vltConexaoThread: TdmConexao;
      begin
        vltHttp := THTTPClient.Create;
        vltConexaoThread := TdmConexao.Create(nil);
        vltQryIntegracao := TFDQuery.Create(nil);
        try
          try
            vltResponse := vltHttp.Get('https://viacep.com.br/ws/' + vlsThreadCep + '/json/');

            if vltResponse.StatusCode = 200 then
            begin
              vltJsonObj := TJSONObject.ParseJSONValue(vltResponse.ContentAsString()) as TJSONObject;
              if Assigned(vltJsonObj) then
              try
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
          end;
        finally
          vltQryIntegracao.Free;
          vltConexaoThread.Free;
          vltHttp.Free;
        end;
      end
    ).Start;

    Result := True;
  finally
    vltQry.Free;
  end;
end;

function TEnderecoDAO.Update(AEndereco: TEndereco): Boolean;
var
  vltQry: TFDQuery;
begin
  vltQry := nil;
  try
    vltQry := TFDQuery.Create(nil);
    vltQry.Connection := FConexao.FDConnection1;

    vltQry.SQL.Add('UPDATE endereco SET dscep = :cep WHERE idpessoa = :pessoa RETURNING idendereco');
    vltQry.ParamByName('cep').AsString := AEndereco.DsCep;
    vltQry.ParamByName('pessoa').AsLargeInt := AEndereco.IdPessoa;
    vltQry.Open;

    if not vltQry.IsEmpty then
    begin
      AEndereco.IdEndereco := vltQry.FieldByName('idendereco').AsLargeInt;

      vltQry.Close;
      vltQry.SQL.Clear;
      vltQry.SQL.Add('DELETE FROM endereco_integracao WHERE idendereco = :id');
      vltQry.ParamByName('id').AsLargeInt := AEndereco.IdEndereco;
      vltQry.ExecSQL;
    end;

    Result := True;
  finally
    vltQry.Free;
  end;
end;

function TEnderecoDAO.Delete(AIdEndereco: Int64): Boolean;
var
  vltQry: TFDQuery;
begin
  vltQry := nil;
  try
    vltQry := TFDQuery.Create(nil);
    vltQry.Connection := FConexao.FDConnection1;
    vltQry.SQL.Add('DELETE FROM endereco WHERE idendereco = :id');
    vltQry.ParamByName('id').AsLargeInt := AIdEndereco;
    vltQry.ExecSQL;
    Result := True;
  finally
    vltQry.Free;
  end;
end;

procedure TEnderecoDAO.ProcessarIntegracoesPendentes;
begin
  // cria uma thread que ficara rodando no bakcground
  TThread.CreateAnonymousThread(
    procedure
    var
      vltHttp: THTTPClient;
      vltResponse: IHTTPResponse;
      vltJsonObj: TJSONObject;
      vltQryBusca, vltQryInt: TFDQuery;
      vltConThread: TdmConexao;
      vliThreadIdEndereco: Int64;
      vlsThreadCep: string;
    begin
      vltHttp := THTTPClient.Create;
      vltConThread := TdmConexao.Create(nil);
      vltQryBusca := TFDQuery.Create(nil);
      vltQryInt := TFDQuery.Create(nil);
      try
        try
          vltQryBusca.Connection := vltConThread.FDConnection1;
          vltQryInt.Connection := vltConThread.FDConnection1;

          vltQryBusca.SQL.Add('SELECT e.idendereco, e.dscep FROM endereco e');
          vltQryBusca.SQL.Add('LEFT JOIN endereco_integracao ei ON e.idendereco = ei.idendereco');
          vltQryBusca.SQL.Add('WHERE ei.idendereco IS NULL');
          vltQryBusca.Open;

          while not vltQryBusca.Eof do
          begin
            vliThreadIdEndereco := vltQryBusca.FieldByName('idendereco').AsLargeInt;
            vlsThreadCep := vltQryBusca.FieldByName('dscep').AsString;

            try
              vltResponse := vltHttp.Get('https://viacep.com.br/ws/' + vlsThreadCep + '/json/');
              if vltResponse.StatusCode = 200 then
              begin
                vltJsonObj := TJSONObject.ParseJSONValue(vltResponse.ContentAsString()) as TJSONObject;
                if Assigned(vltJsonObj) then
                try
                  if vltJsonObj.GetValue('erro') = nil then
                  begin
                    vltQryInt.Close;
                    vltQryInt.SQL.Clear;
                    vltQryInt.SQL.Add('INSERT INTO endereco_integracao (idendereco, dsuf, nmcidade, nmbairro, nmlogradouro, dscomplemento)');
                    vltQryInt.SQL.Add('VALUES (:id, :uf, :cidade, :bairro, :logradouro, :complemento)');

                    vltQryInt.ParamByName('id').AsLargeInt := vliThreadIdEndereco;
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
              // except vazio para o loop não parar
            end;

            // sleep para garantir que o viacep nao ira bloquear em requisiçoes com muitos cadastros.
            Sleep(100);

            vltQryBusca.Next;
          end;
        except
        end;
      finally
        vltQryInt.Free;
        vltQryBusca.Free;
        vltConThread.Free;
        vltHttp.Free;
      end;
    end
  ).Start;
end;

end.
