unit uEnderecoController;

interface

uses
  Horse, System.JSON, REST.Json, System.SysUtils,
  uEndereco, uEnderecoDAO;

procedure Registry;

implementation

// --- Insert de endereço (POST /enderecos) ---
procedure DoInsert(AReq: THorseRequest; ARes: THorseResponse; ANext: TProc);
var
  vltEndereco: TEndereco;
  vltDAO: TEnderecoDAO;
begin
  vltEndereco := nil;
  vltDAO := nil;
  try
    try
      vltEndereco := TJson.JsonToObject<TEndereco>(AReq.Body);
    except
      ARes.Status(400).Send('JSON inválido ou mal formatado.');
      Exit;
    end;

    if not Assigned(vltEndereco) then
    begin
      ARes.Status(400).Send('O corpo da requisição não pode ser vazio.');
      Exit;
    end;

    vltDAO := TEnderecoDAO.Create;
    if vltDAO.Insert(vltEndereco) then
      ARes.Status(201).Send(TJson.ObjectToJsonString(vltEndereco))
    else
      ARes.Status(500).Send('Erro interno ao inserir endereço no banco de dados.');
  finally
    vltEndereco.Free;
    vltDAO.Free;
  end;
end;

// Update de endereço (PUT /enderecos/pessoa/:id) pelo id DA PESSOA
procedure DoUpdate(AReq: THorseRequest; ARes: THorseResponse; ANext: TProc);
var
  vltEndereco: TEndereco;
  vltDAO: TEnderecoDAO;
begin
  vltEndereco := nil;
  vltDAO := nil;
  try
    try
      vltEndereco := TJson.JsonToObject<TEndereco>(AReq.Body);
    except
      ARes.Status(400).Send('JSON inválido ou mal formatado.');
      Exit;
    end;

    if not Assigned(vltEndereco) then
    begin
      ARes.Status(400).Send('O corpo da requisição não pode ser vazio.');
      Exit;
    end;

    try
      // Pega o ID da pessoa na URL e joga no objeto
      vltEndereco.IdPessoa := AReq.Params['id'].ToInt64;
    except
      ARes.Status(400).Send('ID da pessoa inválido na URL.');
      Exit;
    end;

    vltDAO := TEnderecoDAO.Create;
    if vltDAO.Update(vltEndereco) then
      ARes.Status(200).Send('Endereço atualizado com sucesso. Integração pendente foi resetada!')
    else
      ARes.Status(500).Send('Erro interno ao atualizar endereço.');
  finally
    vltEndereco.Free;
    vltDAO.Free;
  end;
end;

// --- Delete de endereço (DELETE /enderecos/:id) ---
// pelo ID DO ENDEREÇO
// Delete Criado para o CRUD ficar completo, mas segundo regras de negocio não deve ter pessoas sem endereço
procedure DoDelete(AReq: THorseRequest; ARes: THorseResponse; ANext: TProc);
var
  vltDAO: TEnderecoDAO;
  vliIdUrl: Int64;
begin
  vltDAO := nil;
  try
    try
      vliIdUrl := AReq.Params['id'].ToInt64;
    except
      ARes.Status(400).Send('ID do endereço inválido na URL.');
      Exit;
    end;

    vltDAO := TEnderecoDAO.Create;
    if vltDAO.Delete(vliIdUrl) then
      ARes.Status(204).Send('')
    else
      ARes.Status(500).Send('Erro interno ao deletar endereço.');
  finally
    vltDAO.Free;
  end;
end;

// --- Thread de integração (POST /enderecos/integrar) ---
procedure DoIntegrarLote(AReq: THorseRequest; ARes: THorseResponse; ANext: TProc);
var
  vltDAO: TEnderecoDAO;
begin
  vltDAO := TEnderecoDAO.Create;
  try
    vltDAO.ProcessarIntegracoesPendentes;
    ARes.Status(202).Send('Rotina de integração iniciada em Background com Threads!');
  finally
    vltDAO.Free;
  end;
end;

// --- Registro das rotas ---
procedure Registry;
begin
  THorse.Post('/enderecos', DoInsert);
  THorse.Put('/enderecos/pessoa/:id', DoUpdate);   // Atualiza pelo ID da Pessoa
  THorse.Delete('/enderecos/:id', DoDelete);       // Deleta pelo ID do Endereço
  THorse.Post('/enderecos/integrar', DoIntegrarLote);
end;

end.
