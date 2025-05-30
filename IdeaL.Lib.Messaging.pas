unit IdeaL.Lib.Messaging;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Messaging;

type
  TMessage = System.Messaging.TMessage;
  TMessageListener = System.Messaging.TMessageListener;

  TMessagingAction = class(System.Messaging.TMessage)
  private
    FReference: string;

    { private declarations }
  protected
    { protected declarations }
  public
    constructor Create(AReference: string); overload; virtual;

    property &Reference: string read FReference write FReference;

    class function Subscribe(const AListenerMethod: TMessageListenerMethod): Integer;
    class procedure Unsubscribe(var AId: Integer; AIsImmediate: Boolean = True);

    class procedure SendAction(Sender: TObject; AReference: string); overload; virtual;
    { public declarations }
  end;

implementation

{ TMessagingAction }

constructor TMessagingAction.Create(AReference: string);
begin
  FReference := AReference;
end;

class procedure TMessagingAction.SendAction(Sender: TObject;
  AReference: string);
begin
  TMessageManager.DefaultManager.SendMessage(Sender, Self.Create(AReference));
end;

class function TMessagingAction.Subscribe(
  const AListenerMethod: TMessageListenerMethod): Integer;
begin
  Result := TMessageManager.DefaultManager.SubscribeToMessage(Self, AListenerMethod);
end;

class procedure TMessagingAction.Unsubscribe(var AId: Integer;
  AIsImmediate: Boolean);
begin
  TMessageManager.DefaultManager.Unsubscribe(Self, AId, AIsImmediate);
  AId := -1; // -1 is my no subscribed default value
end;

end.
