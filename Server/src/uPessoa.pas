unit uPessoa;

interface

uses
  System.SysUtils;

type
  TPessoa = class
  private
    FIdPessoa: Int64;
    FFlNatureza: Integer;
    FDsDocumento: string;
    FNmPrimeiro: string;
    FNmSegundo: string;
    FDtRegistro: TDate;
    FDsCep: string;
  public
    property IdPessoa: Int64 read FIdPessoa write FIdPessoa;
    property FlNatureza: Integer read FFlNatureza write FFlNatureza;
    property DsDocumento: string read FDsDocumento write FDsDocumento;
    property NmPrimeiro: string read FNmPrimeiro write FNmPrimeiro;
    property NmSegundo: string read FNmSegundo write FNmSegundo;
    property DtRegistro: TDate read FDtRegistro write FDtRegistro;
    property DsCep: string read FDsCep write FDsCep;
  end;

implementation

end.
