unit IdeaL.Lib.View.Utils;

interface

uses
  IdeaL.Lib.Utils;

type
  TUtils = class(IdeaL.Lib.Utils.TUtils)
  private
    { private declarations }
  protected
    { protected declarations }
  public
    class var
    TextMessageColor: string;
    TextMessageColorOpacity: string;

    class constructor OnCreate;
    class destructor OnDestroy;
    { public declarations }
  end;

implementation

{ TUtils }

class constructor TUtils.OnCreate;
begin
  TextMessageColor := 'Black';
  TextMessageColor := 'Gray';
end;

class destructor TUtils.OnDestroy;
begin

end;

end.