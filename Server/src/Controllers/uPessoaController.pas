unit uPessoaController;

interface

uses
  Horse, System.JSON, REST.Json, System.SysUtils, System.Generics.Collections,
  uPessoa, uPessoaDAO;

procedure Registry;

implementation

// insert de pessoa (POST /pessoas)
procedure DoInsert(vptReq: THorseRequest; vptRes: THorseResponse; Next: TProc);
var
  vltPessoa: TPessoa;
  vltDAO: TPessoaDAO;
begin
  // converte o objeto
  vltPessoa := TJson.JsonToObject<TPessoa>(vptReq.Body);
  vltDAO := TPessoaDAO.Create;
  try
    if vltDAO.Insert(vltPessoa) then
      vptRes.Status(201).Send(TJson.ObjectToJsonString(vltPessoa))
    else
      vptRes.Status(500).Send('Erro ao inserir pessoa no banco.');
  finally
    vltPessoa.Free;
    vltDAO.Free;
  end;
end;

// update de pessoa (PUT /pessoas/:id) ---
procedure DoUpdate(vptReq: THorseRequest; vptRes: THorseResponse; vptNext: TProc);
var
  vltPessoa: TPessoa;
  vltDAO: TPessoaDAO;
begin
  vltPessoa := TJson.JsonToObject<TPessoa>(vptReq.Body);
  vltDAO := TPessoaDAO.Create;
  try
    // Pega o ID que foi passado na URL (ex: /pessoas/5)
    vltPessoa.IdPessoa := vptReq.Params['id'].ToInt64;

    if vltDAO.Update(vltPessoa) then
      vptRes.Status(200).Send('Pessoa atualizada com sucesso.')
    else
      vptRes.Status(500).Send('Erro ao atualizar pessoa.');
  finally
    vltPessoa.Free;
    vltDAO.Free;
  end;
end;

// delete de pessoa (DELETE /pessoas/:id) ---
procedure DoDelete(vptReq: THorseRequest; vptRes: THorseResponse; vptNext: TProc);
var
  vltDAO: TPessoaDAO;
  vliIdUrl: Int64;
begin
  vltDAO := TPessoaDAO.Create;
  try
    vliIdUrl := vptReq.Params['id'].ToInt64;

    if vltDAO.Delete(vliIdUrl) then
      vptRes.Status(204).Send('') // 204 No Content (padrão REST para Delete)
    else
      vptRes.Status(500).Send('Erro ao deletar pessoa.');
  finally
    vltDAO.Free;
  end;
end;

// insert em lote (POST /pessoas/lote) ---
procedure DoInsertLote(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  vltListaPessoas: TObjectList<TPessoa>;
  vltJsonArray: TJSONArray;
  vliI: Integer;
  vltPessoa: TPessoa;
  vltDAO: TPessoaDAO;
begin
  // recebe um array de json e converte em objectList
  vltJsonArray := TJSONObject.ParseJSONValue(Req.Body) as TJSONArray;
  if not Assigned(vltJsonArray) then
  begin
    Res.Status(400).Send('JSON Inválido. Esperava-se um Array de objetos.');
    Exit;
  end;

  vltListaPessoas := TObjectList<TPessoa>.Create;
  vltDAO := TPessoaDAO.Create;
  try
    // percorre o json populando a lista
    for vliI := 0 to vltJsonArray.Count - 1 do
    begin
      vltPessoa := TJson.JsonToObject<TPessoa>(vltJsonArray.Items[vliI].ToJSON);
      vltListaPessoas.Add(vltPessoa);
    end;

    // Manda a lista inteira
    if vltDAO.InsertLote(vltListaPessoas) then
      Res.Status(201).Send('Lote de ' + vltListaPessoas.Count.ToString + ' pessoas inserido com sucesso!')
    else
      Res.Status(500).Send('Erro ao inserir lote.');
  finally
    vltListaPessoas.Free;
    vltDAO.Free;
    vltJsonArray.Free;
  end;
end;

// registro das rotas
procedure Registry;
begin
  THorse.Post('/pessoas', DoInsert);
  THorse.Put('/pessoas/:id', DoUpdate);
  THorse.Delete('/pessoas/:id', DoDelete);
  THorse.Post('/pessoas/lote', DoInsertLote);
end;

end.
