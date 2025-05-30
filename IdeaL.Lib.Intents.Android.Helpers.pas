unit IdeaL.Lib.Intents.Android.Helpers;

{
 SILVIO SANTOS
 silviosantos.works@gmail.com
 Exemplos de uso de Intents baseados em pesquisas na net
}

interface


uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,

  FMX.DialogService,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,

  {$IFDEF ANDROID}
    Androidapi.Helpers,
    Androidapi.JNI.JavaTypes,
    Androidapi.JNIBridge,
    Androidapi.JNI.GraphicsContentViewText,
    Androidapi.JNI.Net,
    Androidapi.JNI.Os,
    Androidapi.IOUtils,
    Androidapi.JNI.App,
    FMX.Helpers.Android,
  {$ENDIF}
  IdURI;

type
 TIdeaLIntentsAndroidHelpers = class
 private
   { private declarations }
   class var FCommand : string;
   {$IFDEF ANDROID}
   class var FIntent : JIntent;
   class var FPermissions: TJavaObjectArray<JString>;
   {$ENDIF}
 protected
   { protected declarations }
 public
   { public declarations }
   class procedure OpenUrl(AUrl:string);
   class procedure CallDefaultSendEmail(const ADestinatary, AAbstract, AContent: string);
   class procedure PhoneContacts(Sender:TObject);
   class procedure CallPhone(ANum:string);
   class procedure Facebook(AUri:string='');
   class procedure GetPermission(APerm:string);
 published
   { published declarations }
 end;

implementation

{ TIdeaLIntentsAndroidHelpers }

class procedure TIdeaLIntentsAndroidHelpers.OpenUrl(AUrl: string);
begin // Funciona tambem para chamar aplicativos instalados: Ex.: facebook://facebook.com
{$IFDEF ANDROID}
  FIntent := TJIntent.Create;
  FIntent.setAction(TJIntent.JavaClass.ACTION_VIEW);
  FIntent.setData(StrToJURI(AUrl));
  TAndroidHelper.Activity.startActivity(FIntent);
{$ENDIF}
end;

class procedure TIdeaLIntentsAndroidHelpers.CallPhone(ANum: string);
begin  //Ex.: tel://(065)3027-2972
  FCommand:= 'tel://'+ANum;
  try
    FIntent:= TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_CALL);
    //ACTION_DIAL Ir� chamar o Discador j� com o N�mero

    FIntent.setData(TJnet_Uri.JavaClass.parse(
      StringToJString(TIdURI.URLEncode(FCommand))));
    SharedActivity.startActivity(FIntent);
  except on E: Exception do
    ShowMessage(E.Message);
  end;

end;

class procedure TIdeaLIntentsAndroidHelpers.Facebook(AUri: string);
begin // chama o fecebook, podendo passar o perfil no par�metro
  FCommand:= 'facebook://facebook.com/'+AUri;

  try
    FIntent := TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_VIEW);
    FIntent.setData(TJnet_Uri.JavaClass.parse(StringToJString(TIdURI.URLEncode(FCommand))));
    SharedActivity.startActivityForResult(FIntent, 0);
  except on E: Exception do
    ShowMessage(E.Message);
  end;
end;

class procedure TIdeaLIntentsAndroidHelpers.GetPermission(APerm: string);
var
  LPermission, Mens : string;
begin // solicita permiss�o para usar determinada aplica��o
  if Assigned(FPermissions) then FreeAndNil(FPermissions);

  try
    LPermission := 'android.permission.'+APerm;
    FPermissions := TJavaObjectArray<JString>.Create(1);
    FPermissions.Items[0] := StringToJString(LPermission);
    TAndroidHelper.Activity.requestPermissions(FPermissions, 0);
    Application.ProcessMessages;
  except
    Application.ProcessMessages;
  end;
end;

class procedure TIdeaLIntentsAndroidHelpers.CallDefaultSendEmail(
  const ADestinatary, AAbstract, AContent: string);
Var // Abre uma lista de aplicativos, devendo selecionar o app de e-mail
  Recipients: TJavaObjectArray<JString>;
begin
  Try
    FIntent := TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_SEND);
    Recipients:= TJavaObjectArray<JString>.Create(1);
    Recipients.Items[0] := StringToJString(ADestinatary);
    FIntent.putExtra(TJIntent.JavaClass.EXTRA_EMAIL, Recipients);
    FIntent.putExtra(TJIntent.JavaClass.EXTRA_SUBJECT, StringToJString(AAbstract));
    FIntent.putExtra(TJIntent.JavaClass.EXTRA_TEXT, StringToJString(AContent));
    FIntent.setType(StringToJString('plain/html'));  // estava plain/text
    SharedActivity.startActivity(TJIntent.JavaClass.createChooser(
      FIntent,StrToJCharSequence('Selecione o Aplicativo de E-mail'))
    );
  Except on E: Exception do
    ShowMessage(E.Message);
  End;
end;

class procedure TIdeaLIntentsAndroidHelpers.PhoneContacts(Sender: TObject);
begin // Lista os contatos da agenda
  FCommand:= 'content://com.android.contacts/contacts/';
  Try
    FIntent := TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_PICK);
    FIntent.setData(TJnet_Uri.JavaClass.parse(StringToJString(TIdURI.URLEncode(FCommand))));
    SharedActivity.startActivityForResult(FIntent, 0);
  Except on E: Exception do
    ShowMessage(E.Message);
  end;
end;

end.

