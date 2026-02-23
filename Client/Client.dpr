program Client;

uses
  Vcl.Forms,
  uPrincipal in 'uPrincipal.pas' {Form10},
  uPessoaService in 'Service\uPessoaService.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm10, Form10);
  Application.Run;
end.
