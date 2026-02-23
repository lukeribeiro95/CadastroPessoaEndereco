unit uEndereco;

interface

uses
  System.SysUtils;

type
  // Classe que representa a tabela endereco_integracao
  TEnderecoIntegracao = class
  private
    FIdEndereco: Int64;
    FDsUf: string;
    FNmCidade: string;
    FNmBairro: string;
    FNmLogradouro: string;
    FDsComplemento: string;
  public
    property IdEndereco: Int64 read FIdEndereco write FIdEndereco;
    property DsUf: string read FDsUf write FDsUf;
    property NmCidade: string read FNmCidade write FNmCidade;
    property NmBairro: string read FNmBairro write FNmBairro;
    property NmLogradouro: string read FNmLogradouro write FNmLogradouro;
    property DsComplemento: string read FDsComplemento write FDsComplemento;
  end;

  // Classe que representa a tabela endereco
  TEndereco = class
  private
    FIdEndereco: Int64;
    FIdPessoa: Int64;
    FDsCep: string;
    FIntegracao: TEnderecoIntegracao;
  public
    constructor Create;
    destructor Destroy; override;

    property IdEndereco: Int64 read FIdEndereco write FIdEndereco;
    property IdPessoa: Int64 read FIdPessoa write FIdPessoa;
    property DsCep: string read FDsCep write FDsCep;

    property Integracao: TEnderecoIntegracao read FIntegracao write FIntegracao;
  end;

implementation

{ TEndereco }

constructor TEndereco.Create;
begin
  inherited Create;
  FIntegracao := TEnderecoIntegracao.Create;
end;

destructor TEndereco.Destroy;
begin
  FIntegracao.Free;
  inherited Destroy;
end;

end.
