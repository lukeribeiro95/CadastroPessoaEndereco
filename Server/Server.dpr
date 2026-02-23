program Server;

uses
  Vcl.Forms,
  Main.Form in 'src\Main.Form.pas' {FrmVCL},
  uEndereco in 'src\uEndereco.pas',
  uPessoa in 'src\uPessoa.pas',
  uConexao in 'src\DAO\uConexao.pas' {dmConexao: TDataModule},
  uPessoaDAO in 'src\DAO\uPessoaDAO.pas',
  uPessoaController in 'src\Controllers\uPessoaController.pas',
  uEnderecoDAO in 'src\DAO\uEnderecoDAO.pas',
  uEnderecoController in 'src\Controllers\uEnderecoController.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmVCL, FrmVCL);
  Application.CreateForm(TdmConexao, dmConexao);
  Application.Run;
end.
