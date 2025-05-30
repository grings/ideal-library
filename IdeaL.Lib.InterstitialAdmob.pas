unit IdeaL.Lib.InterstitialAdmob;

interface

uses
  System.SysUtils,

  FMX.helpers.android,
  FMX.Platform.android,

  Androidapi.JNI.Net,
  Androidapi.JNI.AdMob,
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Widget,
  Androidapi.JNI.Location,
  Androidapi.JNI.App,
  Androidapi.JNI.Util,
  Androidapi.helpers,
  Androidapi.JNI.PlayServices,
  Androidapi.JNI.Embarcadero,
  Androidapi.JNI.GraphicsContentViewText;

type
  TInterstitialAdmob = class(TJavaLocal, JIAdListener)
  private
    FInterstitial: JInterstitialAd;
    FAdUnitId: string;

    procedure SetAdUnitId(const Value: string);
  public
    constructor Create();
    destructor Destroy(); override;

    property AdUnitId: string read FAdUnitId write SetAdUnitId;

    procedure ShowAdmob();

    procedure OnAdClosed; cdecl;
    procedure OnAdFailedToLoad(AErrorCode: Integer); cdecl;
    procedure OnAdLeftApplication; cdecl;
    procedure OnAdOpened; cdecl;
    procedure OnAdLoaded; cdecl;
  end;

implementation

{ TMyAdViewListener }

constructor TInterstitialAdmob.Create;
begin
  inherited Create;
  FAdUnitId := '';
end;

destructor TInterstitialAdmob.Destroy;
begin

  inherited;
end;

procedure TInterstitialAdmob.OnAdClosed;
begin

end;

procedure TInterstitialAdmob.OnAdFailedToLoad(AErrorCode: Integer);
begin

end;

procedure TInterstitialAdmob.OnAdLeftApplication;
begin

end;

procedure TInterstitialAdmob.OnAdLoaded;
begin
  FInterstitial.show;
end;

procedure TInterstitialAdmob.OnAdOpened;
begin

end;

procedure TInterstitialAdmob.SetAdUnitId(const Value: string);
begin
  FAdUnitId := Value;
  FInterstitial := TJInterstitialAd.JavaClass.init(MainActivity);
  // Aqui você vai colocar o código DO INTERSTITIAL criado no Admob
  FInterstitial.setAdUnitId(StringToJString(Value));
end;

procedure TInterstitialAdmob.ShowAdmob;
var
  LADRequestBuilder: JAdRequest_Builder;
  LadRequest: JAdRequest;
begin
  if(FAdUnitId.Trim.IsEmpty)then
    raise Exception.Create('FAdUnitId is empty');

  LADRequestBuilder := TJAdRequest_Builder.Create;
  {$IFDEF DEBUG}
  LADRequestBuilder.addTestDevice(MainActivity.getDeviceID);
  {$ENDIF}
  LadRequest := LADRequestBuilder.build();
  CallInUIThread(
    procedure
    begin
      FInterStitial.setAdListener(TJAdListenerAdapter.JavaClass.init
        (Self));
      FInterStitial.loadAd(LadRequest);
    end);
end;

end.

{
Modo de utilizar:
Declare um objeto FInterstitialAdmob: TInterstitialAdmob;
Realize o Create do msm FInterstitialAdmob := TInterstitialAdmob.Create;
Preencha o FAdUnitId: FInterstitialAdmob.AdUnitId := '<ID do Interstitial>';
Faca a chamada dessa forma FInterstitialAdmob.ShowAdmob;
Lembre-se de destruir o objeto qnd necessario: if(Assigned(FInterstitialAdmob))then FInterstitialAdmob.DisposeOf;

Lembrando que sera necessario a configuracao correta do AndroidManifest.template,xml

Adicione dentro do <application:
    <meta-data
			android:name="com.google.android.gms.ads.APPLICATION_ID"
			android:value="Troque isso aqui pelo seu ID do Admob"
		/>
}
