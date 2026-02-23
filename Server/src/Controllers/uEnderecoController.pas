unit uEnderecoController;

interface

uses
  Horse, System.JSON, REST.Json, System.SysUtils,
  uEndereco, uEnderecoDAO;

procedure Registry;

implementation

// insert de endereço (POST /enderecos) ---
procedure DoInsert(vptReq: THorseRequest; vptRes: THorseResponse; vptNext: TProc);
var
  vltEndereco: TEndereco;
  vltDAO: TEnderecoDAO;
begin
  //converte o objeto
  vltEndereco := TJson.JsonToObject<TEndereco>(vptReq.Body);
  vltDAO := TEnderecoDAO.Create;
  try
    if vltDAO.Insert(vltEndereco) then
      vptRes.Status(201).Send(TJson.ObjectToJsonString(vltEndereco))
    else
      vptRes.Status(500).Send('Erro ao inserir endereço no banco.');
  finally
    vltEndereco.Free;
    vltDAO.Free;
  end;
end;

//thread de integração (POST /enderecos/integrar) ---
procedure DoIntegrarLote(vptReq: THorseRequest; vptRes: THorseResponse; vptNext: TProc);
var
  vltDAO: TEnderecoDAO;
begin
  vltDAO := TEnderecoDAO.Create;
  try
    vltDAO.ProcessarIntegracoesPendentes;
    vptRes.Status(202).Send('Rotina de integração iniciada em Background com Threads!');
  finally
    vltDAO.Free;
  end;
end;

// registro das rotas
procedure Registry;
begin
  THorse.Post('/enderecos', DoInsert);
  THorse.Post('/enderecos/integrar', DoIntegrarLote);
end;

end.
