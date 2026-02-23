unit uPessoaDAO;

interface

uses
  System.SysUtils, System.Generics.Collections, System.Classes,
  System.Net.HttpClient, System.JSON, FireDAC.Comp.Client, FireDAC.DApt,
  uPessoa, uConexao;

type
  TPessoaDAO = class
  private
    FConexao: TdmConexao;
  public
    constructor Create;
    destructor Destroy; override;

    function Insert(APessoa: TPessoa): Boolean;
    function Update(APessoa: TPessoa): Boolean;
    function Delete(AIdPessoa: Int64): Boolean;

    // O método Sênior para inserir 50 mil registros em milissegundos
    function InsertLote(ALista: TObjectList<TPessoa>): Boolean;
  end;

implementation

{ TPessoaDAO }

constructor TPessoaDAO.Create;
begin
  // Instancia a nossa conexão com o banco de dados
  FConexao := TdmConexao.Create(nil);
end;

destructor TPessoaDAO.Destroy;
begin
  // Evita vazamento de memória destruindo a conexão adequadamente
  FConexao.Free;
  inherited;
end;

function TPessoaDAO.Insert(APessoa: TPessoa): Boolean;
var
  Qry: TFDQuery;
  ThreadCep: string;
  ThreadIdEndereco: Int64;
begin
  Result := False;
  // Regra de Integridade: Se não mandou CEP, nem tenta salvar!
  if APessoa.DsCep.Trim = '' then Exit;

  Qry := TFDQuery.Create(nil);

  // INICIA A TRANSAÇÃO (Garante a integridade - Tudo ou Nada)
  FConexao.FDConnection1.StartTransaction;
  try
    Qry.Connection := FConexao.FDConnection1;

    // 1. Insere a Pessoa
    Qry.SQL.Add('INSERT INTO pessoa (flnatureza, dsdocumento, nmprimeiro, nmsegundo, dtregistro)');
    Qry.SQL.Add('VALUES (:natureza, :documento, :primeiro, :segundo, :registro)');
    Qry.SQL.Add('RETURNING idpessoa');

    Qry.ParamByName('natureza').AsInteger := APessoa.FlNatureza;
    Qry.ParamByName('documento').AsString := APessoa.DsDocumento;
    Qry.ParamByName('primeiro').AsString := APessoa.NmPrimeiro;
    Qry.ParamByName('segundo').AsString := APessoa.NmSegundo;
    Qry.ParamByName('registro').AsDate := APessoa.DtRegistro;
    Qry.Open;

    APessoa.IdPessoa := Qry.FieldByName('idpessoa').AsLargeInt;

    // 2. Insere o Endereço vinculado àquela Pessoa na mesma transação!
    Qry.Close;
    Qry.SQL.Clear;
    Qry.SQL.Add('INSERT INTO endereco (idpessoa, dscep) VALUES (:pessoa, :cep) RETURNING idendereco');
    Qry.ParamByName('pessoa').AsLargeInt := APessoa.IdPessoa;
    Qry.ParamByName('cep').AsString := APessoa.DsCep;
    Qry.Open;

    ThreadIdEndereco := Qry.FieldByName('idendereco').AsLargeInt;
    ThreadCep := APessoa.DsCep;

    // CONFIRMA TUDO NO BANCO (Neste momento, Pessoa e Endereço são gravados juntos!)
    FConexao.FDConnection1.Commit;

    // 3. Dispara a Thread do ViaCEP
    TThread.CreateAnonymousThread(
      procedure
      var
        Http: THTTPClient;
        Response: IHTTPResponse;
        JsonObj: TJSONObject;
        QryInt: TFDQuery;
        ConThread: TdmConexao;
      begin
        Http := THTTPClient.Create;
        ConThread := TdmConexao.Create(nil);
        QryInt := TFDQuery.Create(nil);
        try
          try
            Response := Http.Get('https://viacep.com.br/ws/' + ThreadCep + '/json/');
            if Response.StatusCode = 200 then
            begin
              JsonObj := TJSONObject.ParseJSONValue(Response.ContentAsString()) as TJSONObject;
              if Assigned(JsonObj) then
              try
                if JsonObj.GetValue('erro') = nil then
                begin
                  QryInt.Connection := ConThread.FDConnection1;
                  QryInt.SQL.Add('INSERT INTO endereco_integracao (idendereco, dsuf, nmcidade, nmbairro, nmlogradouro, dscomplemento)');
                  QryInt.SQL.Add('VALUES (:id, :uf, :cidade, :bairro, :logradouro, :complemento)');

                  QryInt.ParamByName('id').AsLargeInt := ThreadIdEndereco;
                  QryInt.ParamByName('uf').AsString := JsonObj.GetValue('uf').Value;
                  QryInt.ParamByName('cidade').AsString := JsonObj.GetValue('localidade').Value;
                  QryInt.ParamByName('bairro').AsString := JsonObj.GetValue('bairro').Value;
                  QryInt.ParamByName('logradouro').AsString := JsonObj.GetValue('logradouro').Value;

                  if JsonObj.GetValue('complemento') <> nil then
                    QryInt.ParamByName('complemento').AsString := JsonObj.GetValue('complemento').Value
                  else
                    QryInt.ParamByName('complemento').AsString := '';

                  QryInt.ExecSQL;
                end;
              finally
                JsonObj.Free;
              end;
            end;
          except
          end;
        finally
          QryInt.Free;
          ConThread.Free;
          Http.Free;
        end;
      end
    ).Start;

    Result := True;
  except
    // SE QUALQUER COISA DER ERRO, DESFAZ A PESSOA E O ENDEREÇO (Garante a Integridade!)
    FConexao.FDConnection1.Rollback;
    Result := False;
  end;
  Qry.Free;
end;

function TPessoaDAO.Update(APessoa: TPessoa): Boolean;
var
  Qry: TFDQuery;
begin
  Qry := TFDQuery.Create(nil);
  try
    Qry.Connection := FConexao.FDConnection1;
    Qry.SQL.Add('UPDATE pessoa SET flnatureza = :natureza, dsdocumento = :documento,');
    Qry.SQL.Add('nmprimeiro = :primeiro, nmsegundo = :segundo, dtregistro = :registro');
    Qry.SQL.Add('WHERE idpessoa = :id');

    Qry.ParamByName('natureza').AsInteger := APessoa.FlNatureza;
    Qry.ParamByName('documento').AsString := APessoa.DsDocumento;
    Qry.ParamByName('primeiro').AsString := APessoa.NmPrimeiro;
    Qry.ParamByName('segundo').AsString := APessoa.NmSegundo;
    Qry.ParamByName('registro').AsDate := APessoa.DtRegistro;
    Qry.ParamByName('id').AsLargeInt := APessoa.IdPessoa;

    Qry.ExecSQL;
    Result := True;
  finally
    Qry.Free;
  end;
end;

function TPessoaDAO.Delete(AIdPessoa: Int64): Boolean;
var
  Qry: TFDQuery;
begin
  Qry := TFDQuery.Create(nil);
  try
    Qry.Connection := FConexao.FDConnection1;
    Qry.SQL.Add('DELETE FROM pessoa WHERE idpessoa = :id');
    Qry.ParamByName('id').AsLargeInt := AIdPessoa;

    Qry.ExecSQL;
    Result := True;
  finally
    Qry.Free;
  end;
end;

function TPessoaDAO.InsertLote(ALista: TObjectList<TPessoa>): Boolean;
var
  Qry: TFDQuery;
  I: Integer;
begin
  Result := False;
  if ALista.Count = 0 then Exit;

  Qry := TFDQuery.Create(nil);
  try
    Qry.Connection := FConexao.FDConnection1;
    Qry.SQL.Add('INSERT INTO pessoa (flnatureza, dsdocumento, nmprimeiro, nmsegundo, dtregistro)');
    Qry.SQL.Add('VALUES (:natureza, :documento, :primeiro, :segundo, :registro)');

    // Configura o Array DML para o tamanho exato da nossa lista (ex: 50.000)
    Qry.Params.ArraySize := ALista.Count;

    // Alimenta os arrays com os dados da lista
    for I := 0 to ALista.Count - 1 do
    begin
      Qry.ParamByName('natureza').AsIntegers[I] := ALista[I].FlNatureza;
      Qry.ParamByName('documento').AsStrings[I] := ALista[I].DsDocumento;
      Qry.ParamByName('primeiro').AsStrings[I] := ALista[I].NmPrimeiro;
      Qry.ParamByName('segundo').AsStrings[I] := ALista[I].NmSegundo;
      Qry.ParamByName('registro').AsDates[I] := ALista[I].DtRegistro;
    end;

    // Executa as 50.000 inserções em uma única chamada de rede!
    Qry.Execute(ALista.Count, 0);
    Result := True;
  finally
    Qry.Free;
  end;
end;

end.
