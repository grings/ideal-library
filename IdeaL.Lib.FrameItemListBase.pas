unit IdeaL.Lib.FrameItemListBase;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Objects;

type
  TFrameItemListBase = class(TFrame)
    lytBackgroud: TLayout;
    rctBackground: TRectangle;
    rctClient: TRectangle;
    lneBottom: TLine;
    lytClient: TLayout;
    procedure OnClickDefault(Sender: TObject);
  private
    FIdentify: string;
    procedure SetIdentify(const Value: string);
    { Private declarations }
  public
    property Identify: string read FIdentify write SetIdentify;

    procedure SetOnClickDefault(Value: TNotifyEvent);
    procedure SetOnTapDefault(Value: TTapEvent);
    { Public declarations }
  end;

implementation

{$R *.fmx}

{ TFrameItemListBase }

procedure TFrameItemListBase.OnClickDefault(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  if (Sender.InheritsFrom(TControl)) and (Assigned(TControl(Sender).OnTap)) then
    TControl(Sender).OnTap(Sender, TPointF.Create(0, 0));
{$ENDIF}
end;

procedure TFrameItemListBase.SetIdentify(const Value: string);
begin
  FIdentify := Value;
end;

procedure TFrameItemListBase.SetOnClickDefault(Value: TNotifyEvent);
begin
  rctBackground.OnClick := Value;
end;

procedure TFrameItemListBase.SetOnTapDefault(Value: TTapEvent);
begin
  rctBackground.OnTap := Value;
end;

end.
