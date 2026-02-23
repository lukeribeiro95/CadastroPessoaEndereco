unit uPessoaController;

interface

uses
  Horse, System.JSON, REST.Json, System.SysUtils, System.Generics.Collections,
  uPessoa, uPessoaDAO;

procedure Registry;

implementation

// --- insert de pessoa (POST /pessoas) ---
procedure DoInsert(AReq: THorseRequest; ARes: THorseResponse; ANext: TProc);
var
  vltPessoa: TPessoa;
  vltDAO: TPessoaDAO;
begin
  vltPessoa := nil;
  vltDAO := nil;

  try
    try
      // Tenta converter o objeto
      vltPessoa := TJson.JsonToObject<TPessoa>(AReq.Body);
    except
      ARes.Status(400).Send('JSON inválido ou mal formatado.');
      Exit;
    end;

    if not Assigned(vltPessoa) then
    begin
      ARes.Status(400).Send('O corpo da requisição não pode ser vazio.');
      Exit;
    end;

    vltDAO := TPessoaDAO.Create;

    if vltDAO.Insert(vltPessoa) then
      ARes.Status(201).Send(TJson.ObjectToJsonString(vltPessoa))
    else
      ARes.Status(500).Send('Erro interno ao inserir pessoa no banco.');

  finally
    vltPessoa.Free;
    vltDAO.Free;
  end;
end;

// --- update de pessoa (PUT /pessoas/:id) ---
procedure DoUpdate(AReq: THorseRequest; ARes: THorseResponse; ANext: TProc);
var
  vltPessoa: TPessoa;
  vltDAO: TPessoaDAO;
begin
  vltPessoa := nil;
  vltDAO := nil;

  try
    try
      vltPessoa := TJson.JsonToObject<TPessoa>(AReq.Body);
    except
      ARes.Status(400).Send('JSON inválido ou mal formatado.');
      Exit;
    end;

    if not Assigned(vltPessoa) then
    begin
      ARes.Status(400).Send('O corpo da requisição não pode ser vazio.');
      Exit;
    end;

    try
      // confirma apenas numeros no id
      vltPessoa.IdPessoa := AReq.Params['id'].ToInt64;
    except
      ARes.Status(400).Send('ID da pessoa inválido na URL.');
      Exit;
    end;

    vltDAO := TPessoaDAO.Create;
    if vltDAO.Update(vltPessoa) then
      ARes.Status(200).Send('Pessoa atualizada com sucesso.')
    else
      ARes.Status(500).Send('Erro interno ao atualizar pessoa.');

  finally
    vltPessoa.Free;
    vltDAO.Free;
  end;
end;

// --- delete de pessoa (DELETE /pessoas/:id) ---
procedure DoDelete(AReq: THorseRequest; ARes: THorseResponse; ANext: TProc);
var
  vltDAO: TPessoaDAO;
  vliIdUrl: Int64;
begin
  vltDAO := nil;
  try
    try
      // confirma apenas numeros no id
      vliIdUrl := AReq.Params['id'].ToInt64;
    except
      ARes.Status(400).Send('ID da pessoa inválido na URL.');
      Exit;
    end;

    vltDAO := TPessoaDAO.Create;
    if vltDAO.Delete(vliIdUrl) then
      ARes.Status(204).Send('')
    else
      ARes.Status(500).Send('Erro interno ao deletar pessoa.');

  finally
    vltDAO.Free;
  end;
end;

// --- insert em lote (POST /pessoas/lote) ---
procedure DoInsertLote(AReq: THorseRequest; ARes: THorseResponse; ANext: TProc);
var
  vltDAO: TPessoaDAO;
  vltListaPessoas: TObjectList<TPessoa>;
  vliQtdInserida: Integer;
  vltJsonArray: TJSONArray;
  vltJsonVal: TJSONValue;
  vltJsonRes: TJSONObject;
  vltJsonRetorno: TJSONObject;
  vlsTextoJson: string;
  vliI: Integer;
begin
  vltListaPessoas := nil;
  vltDAO := nil;
  vltJsonVal := nil;

  try
    try

      vltJsonVal := TJSONObject.ParseJSONValue(AReq.Body);

      if (not Assigned(vltJsonVal)) or (not (vltJsonVal is TJSONArray)) then
      begin
        ARes.Status(400).Send(TJSONObject.Create.AddPair('erro', 'Esperava-se um Array JSON no corpo da requisição.'));
        Exit;
      end;

      vltJsonArray := vltJsonVal as TJSONArray;
      vltListaPessoas := TObjectList<TPessoa>.Create;


      for vliI := 0 to vltJsonArray.Count - 1 do
      begin
        // Converte cada item do array para o objeto
        vltListaPessoas.Add(TJson.JsonToObject<TPessoa>(vltJsonArray.Items[vliI].ToJSON));
      end;

    except
      on E: Exception do
      begin
        ARes.Status(400).Send(TJSONObject.Create.AddPair('erro', 'Falha na conversão do JSON: ' + E.Message));
        Exit;
      end;
    end;

    vltDAO := TPessoaDAO.Create;
    vliQtdInserida := vltDAO.InsertLote(vltListaPessoas);

    vltJsonRetorno := TJSONObject.Create;
    try
      if vliQtdInserida > 0 then
      begin
        vltJsonRetorno.AddPair('mensagem', Format('Processamento concluído. %d pessoas foram inseridas com sucesso.', [vliQtdInserida]));
        vlsTextoJson := vltJsonRetorno.ToJSON;

        ARes.ContentType('application/json')
            .Status(201)
            .Send(vlsTextoJson);
      end
      else if vliQtdInserida = 0 then
      begin
        vltJsonRetorno.AddPair('erro', 'Nenhum registro foi inserido. Certifique-se de que os objetos possuem o campo "dscep".');
        vlsTextoJson := vltJsonRetorno.ToJSON;

        ARes.ContentType('application/json')
            .Status(400)
            .Send(vlsTextoJson);
      end
      else
      begin
        vltJsonRetorno.AddPair('erro', 'Falha interna ao processar lote no banco de dados.');
        vlsTextoJson := vltJsonRetorno.ToJSON;

        ARes.ContentType('application/json')
            .Status(500)
            .Send(vlsTextoJson);
      end;
    finally
      vltJsonRetorno.Free;
    end;

  finally
    vltDAO.Free;
    vltListaPessoas.Free;
    if Assigned(vltJsonVal) then vltJsonVal.Free;
  end;
end;

// --- registro das rotas ---
procedure Registry;
begin
  THorse.Post('/pessoas', DoInsert);
  THorse.Put('/pessoas/:id', DoUpdate);
  THorse.Delete('/pessoas/:id', DoDelete);
  THorse.Post('/pessoas/lote', DoInsertLote);
end;

end.
