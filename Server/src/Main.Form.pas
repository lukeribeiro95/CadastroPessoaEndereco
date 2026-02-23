unit Main.Form;

interface

uses
  Winapi.Windows, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, System.SysUtils;

type
  TFrmVCL = class(TForm)
    btnStop: TBitBtn;
    btnStart: TBitBtn;
    Label1: TLabel;
    edtPort: TEdit;
    procedure btnStopClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure Status;
    procedure Start;
    procedure Stop;
  end;

var
  FrmVCL: TFrmVCL;

implementation

uses
  Horse,
  uPessoaController,
  uEnderecoController;

{$R *.dfm}

procedure TFrmVCL.FormCreate(Sender: TObject);
begin
  uPessoaController.Registry;
  uEnderecoController.Registry;
  Status;
end;

procedure TFrmVCL.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if THorse.IsRunning then
    Stop;
end;

procedure TFrmVCL.Start;
begin
  try
    THorse.Listen(StrToIntDef(edtPort.Text, 8080));
  except
    on vltE: Exception do
    begin
      ShowMessage('Não foi possível iniciar o servidor na porta ' + edtPort.Text + '.' + sLineBreak +
                  'Detalhe do erro: ' + vltE.Message);
    end;
  end;
end;

procedure TFrmVCL.Stop;
begin
  if THorse.IsRunning then
    THorse.StopListen;
end;

procedure TFrmVCL.Status;
begin
  btnStop.Enabled := THorse.IsRunning;
  btnStart.Enabled := not THorse.IsRunning;
  edtPort.Enabled := not THorse.IsRunning;
end;

procedure TFrmVCL.btnStartClick(Sender: TObject);
begin
  Start;
  Status;
end;

procedure TFrmVCL.btnStopClick(Sender: TObject);
begin
  Stop;
  Status;
end;

end.
