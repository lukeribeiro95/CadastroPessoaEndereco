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
    function InsertLote(ALista: TObjectList<TPessoa>): Integer;
  end;

implementation

{ TPessoaDAO }

constructor TPessoaDAO.Create;
begin
  // Instancia o banco de dados
  FConexao := TdmConexao.Create(nil);
end;

destructor TPessoaDAO.Destroy;
begin
  FConexao.Free;
  inherited;
end;

function TPessoaDAO.Insert(APessoa: TPessoa): Boolean;
var
  vltQry: TFDQuery;
  vlsThreadCep: string;
  vliThreadIdEndereco: Int64;
begin
  Result := False;
  // sem cep nao salva
  if APessoa.DsCep.Trim = '' then Exit;

  vltQry := nil;

  // INICIA A TRANSAÇÃO
  FConexao.FDConnection1.StartTransaction;
  try
    vltQry := TFDQuery.Create(nil);
    try
      vltQry.Connection := FConexao.FDConnection1;

      //Insere a Pessoa
      vltQry.SQL.Add('INSERT INTO pessoa (flnatureza, dsdocumento, nmprimeiro, nmsegundo, dtregistro)');
      vltQry.SQL.Add('VALUES (:natureza, :documento, :primeiro, :segundo, :registro)');
      vltQry.SQL.Add('RETURNING idpessoa');

      vltQry.ParamByName('natureza').AsInteger := APessoa.FlNatureza;
      vltQry.ParamByName('documento').AsString := APessoa.DsDocumento;
      vltQry.ParamByName('primeiro').AsString := APessoa.NmPrimeiro;
      vltQry.ParamByName('segundo').AsString := APessoa.NmSegundo;
      vltQry.ParamByName('registro').AsDate := APessoa.DtRegistro;
      vltQry.Open;

      APessoa.IdPessoa := vltQry.FieldByName('idpessoa').AsLargeInt;

      // Insere o Endereço vinculado a mesma pessoa
      vltQry.Close;
      vltQry.SQL.Clear;
      vltQry.SQL.Add('INSERT INTO endereco (idpessoa, dscep) VALUES (:pessoa, :cep) RETURNING idendereco');
      vltQry.ParamByName('pessoa').AsLargeInt := APessoa.IdPessoa;
      vltQry.ParamByName('cep').AsString := APessoa.DsCep;
      vltQry.Open;

      vliThreadIdEndereco := vltQry.FieldByName('idendereco').AsLargeInt;
      vlsThreadCep := APessoa.DsCep;

      // commit dos dois
      FConexao.FDConnection1.Commit;

      // inicia a thread de CEP
      TThread.CreateAnonymousThread(
        procedure
        var
          vltHttp: THTTPClient;
          vltResponse: IHTTPResponse;
          vltJsonObj: TJSONObject;
          vltQryInt: TFDQuery;
          vltConThread: TdmConexao;
        begin
          vltHttp := THTTPClient.Create;
          vltConThread := TdmConexao.Create(nil);
          vltQryInt := TFDQuery.Create(nil);
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
                    vltQryInt.Connection := vltConThread.FDConnection1;
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
            end;
          finally
            vltQryInt.Free;
            vltConThread.Free;
            vltHttp.Free;
          end;
        end
      ).Start;

      Result := True;
    finally
      vltQry.Free;
    end;
  except
    // Se algo deu errado desfaz
    if FConexao.FDConnection1.InTransaction then
      FConexao.FDConnection1.Rollback;
    Result := False;
  end;
end;

function TPessoaDAO.Update(APessoa: TPessoa): Boolean;
var
  vltQry: TFDQuery;
begin
  vltQry := nil;
  try
    vltQry := TFDQuery.Create(nil);
    vltQry.Connection := FConexao.FDConnection1;
    vltQry.SQL.Add('UPDATE pessoa SET flnatureza = :natureza, dsdocumento = :documento,');
    vltQry.SQL.Add('nmprimeiro = :primeiro, nmsegundo = :segundo, dtregistro = :registro');
    vltQry.SQL.Add('WHERE idpessoa = :id');

    vltQry.ParamByName('natureza').AsInteger := APessoa.FlNatureza;
    vltQry.ParamByName('documento').AsString := APessoa.DsDocumento;
    vltQry.ParamByName('primeiro').AsString := APessoa.NmPrimeiro;
    vltQry.ParamByName('segundo').AsString := APessoa.NmSegundo;
    vltQry.ParamByName('registro').AsDate := APessoa.DtRegistro;
    vltQry.ParamByName('id').AsLargeInt := APessoa.IdPessoa;

    vltQry.ExecSQL;
    Result := True;
  finally
    vltQry.Free;
  end;
end;

function TPessoaDAO.Delete(AIdPessoa: Int64): Boolean;
var
  vltQry: TFDQuery;
begin
  vltQry := nil;
  try
    vltQry := TFDQuery.Create(nil);
    vltQry.Connection := FConexao.FDConnection1;
    vltQry.SQL.Add('DELETE FROM pessoa WHERE idpessoa = :id');
    vltQry.ParamByName('id').AsLargeInt := AIdPessoa;

    vltQry.ExecSQL;
    Result := True;
  finally
    vltQry.Free;
  end;
end;

//inicial havia feito o insert em lote utilizando um Array e dando insert com uma unica chamada
//mas como tem integridade de uma pessoa não poder ficar sem endereço esse metodo nao funcionaria.
//function TPessoaDAO.InsertLote(ALista: TObjectList<TPessoa>): Boolean;
//var
//  vltQry: TFDQuery;
//  vliI: Integer;
//begin
//  Result := False;
//  if ALista.Count = 0 then Exit;
//
//  vltQry := nil;
//
//  FConexao.FDConnection1.StartTransaction;
//  try
//    vltQry := TFDQuery.Create(nil);
//    try
//      vltQry.Connection := FConexao.FDConnection1;
//      vltQry.SQL.Add('INSERT INTO pessoa (flnatureza, dsdocumento, nmprimeiro, nmsegundo, dtregistro)');
//      vltQry.SQL.Add('VALUES (:natureza, :documento, :primeiro, :segundo, :registro)');
//
//      vltQry.Params.ArraySize := ALista.Count;
//
//      // Alimenta os arrays com os dados da lista
//      for vliI := 0 to ALista.Count - 1 do
//      begin
//        vltQry.ParamByName('natureza').AsIntegers[vliI] := ALista[vliI].FlNatureza;
//        vltQry.ParamByName('documento').AsStrings[vliI] := ALista[vliI].DsDocumento;
//        vltQry.ParamByName('primeiro').AsStrings[vliI] := ALista[vliI].NmPrimeiro;
//        vltQry.ParamByName('segundo').AsStrings[vliI] := ALista[vliI].NmSegundo;
//        vltQry.ParamByName('registro').AsDates[vliI] := ALista[vliI].DtRegistro;
//      end;
//
//      // Executa as inserções em uma única chamada
//      vltQry.Execute(ALista.Count, 0);
//
//      FConexao.FDConnection1.Commit;
//      Result := True;
//    finally
//      vltQry.Free;
//    end;
//  except
//    if FConexao.FDConnection1.InTransaction then
//      FConexao.FDConnection1.Rollback;
//    Result := False;
//  end;
//end;

function TPessoaDAO.InsertLote(ALista: TObjectList<TPessoa>): Integer;
var
  vltQryPessoa, vltQryEndereco: TFDQuery;
  vliI: Integer;
  vliContadorSucesso: Integer;
begin
  Result := 0;
  vliContadorSucesso := 0;

  if (not Assigned(ALista)) or (ALista.Count = 0) then
    Exit;

  vltQryPessoa := nil;
  vltQryEndereco := nil;

  FConexao.FDConnection1.StartTransaction;
  try
    vltQryPessoa := TFDQuery.Create(nil);
    vltQryEndereco := TFDQuery.Create(nil);

    vltQryPessoa.Connection := FConexao.FDConnection1;
    vltQryEndereco.Connection := FConexao.FDConnection1;

    vltQryPessoa.SQL.Add('INSERT INTO pessoa (flnatureza, dsdocumento, nmprimeiro, nmsegundo, dtregistro)');
    vltQryPessoa.SQL.Add('VALUES (:natureza, :documento, :primeiro, :segundo, :registro) RETURNING idpessoa');

    vltQryEndereco.SQL.Add('INSERT INTO endereco (idpessoa, dscep) VALUES (:pessoa, :cep)');

    for vliI := 0 to ALista.Count - 1 do
    begin
      // se veio sem cep nao insere
      if ALista[vliI].DsCep.Trim = '' then
        Continue;

      vltQryPessoa.ParamByName('natureza').AsInteger := ALista[vliI].FlNatureza;
      vltQryPessoa.ParamByName('documento').AsString := ALista[vliI].DsDocumento;
      vltQryPessoa.ParamByName('primeiro').AsString := ALista[vliI].NmPrimeiro;
      vltQryPessoa.ParamByName('segundo').AsString := ALista[vliI].NmSegundo;
      vltQryPessoa.ParamByName('registro').AsDate := ALista[vliI].DtRegistro;
      vltQryPessoa.Open;

      ALista[vliI].IdPessoa := vltQryPessoa.FieldByName('idpessoa').AsLargeInt;
      vltQryPessoa.Close;

      vltQryEndereco.ParamByName('pessoa').AsLargeInt := ALista[vliI].IdPessoa;
      vltQryEndereco.ParamByName('cep').AsString := ALista[vliI].DsCep;
      vltQryEndereco.ExecSQL;

      Inc(vliContadorSucesso);
    end;

    FConexao.FDConnection1.Commit;
    Result := vliContadorSucesso;

  except
    if FConexao.FDConnection1.InTransaction then
      FConexao.FDConnection1.Rollback;
    Result := -1;
  end;

  if Assigned(vltQryPessoa) then vltQryPessoa.Free;
  if Assigned(vltQryEndereco) then vltQryEndereco.Free;
end;

end.
