2014/02/03現在有効な記事です

ALSの身内のために購入したのでメモ。

https://github.com/EyeTribe/tet-csharp-samples/blob/master/TETControls/Cursor/CursorControl.cs
公式のサンプルの改変

## 環境
- Windows7 x64
- Visual Studio 2012 Express for Desktop
- The Eye Tribe Tracker software (V0.9.26)

## 前準備
```
Visual Studioで作成する場合，プロジェクトに「TETCSharpClient.dll」と「Newtonsoft.Json.dll」を参照に追加しておく．
デフォルトでは「C:\Program Files (x86)\EyeTribe\Client」の中にある．
マウスカーソルを操作するので「System.Windows.Forms」と「System.Drawing」も必要．

あとは，EyeTribe Serverを立ち上げておけばいい．
```
- http://romichi.ivory.ne.jp/wordpress/?p=739
- その後Eye Tribe UIを起動して先にキャリブレーションしておいてから
- このコードをビルドして起動してください

## サンプルコード
- 右目閉じると右クリック
- 左目閉じると左クリック

```csharp
using System;
using TETCSharpClient;
using TETCSharpClient.Data;
using System.Drawing;
using System.Windows.Forms;
using System.Runtime.InteropServices;

public class CursorControl:IGazeListener {
  public bool Enabled { get; set; }
  public bool Smooth { get; set; }
  public Screen ActiveScreen { get; set; }

  public CursorControl() : this(Screen.PrimaryScreen, false, false) { }
  public CursorControl(Screen screen, bool enabled, bool smooth) {
    GazeManager.Instance.AddGazeListener(this);
    ActiveScreen = screen;
    Enabled = enabled;
    Smooth = smooth;
  }

  public void OnGazeUpdate(GazeData gazeData) {

    if(!Enabled) {
      return;
    }

    // start or stop tracking lost animation
    if((gazeData.State & GazeData.STATE_TRACKING_GAZE) == 0 &&
       (gazeData.State & GazeData.STATE_TRACKING_PRESENCE) == 0) {
      Console.WriteLine("start or stop tracking lost animation");
      return;
    }

    // tracking coordinates
    var x = ActiveScreen.Bounds.X;
    var y = ActiveScreen.Bounds.Y;
    var gX = Smooth ? gazeData.SmoothedCoordinates.X : gazeData.RawCoordinates.X;
    var gY = Smooth ? gazeData.SmoothedCoordinates.Y : gazeData.RawCoordinates.Y;
    var screenX = (int)Math.Round(x + gX, 0);
    var screenY = (int)Math.Round(y + gY, 0);

    Console.WriteLine(gX + "," + gY);
    if(gazeData.LeftEye.SmoothedCoordinates.X == 0 && gazeData.LeftEye.SmoothedCoordinates.Y == 0 &&
       gazeData.RightEye.SmoothedCoordinates.X == 0 && gazeData.RightEye.SmoothedCoordinates.Y == 0) {
      Console.WriteLine("both eyes close");
    } else if(gazeData.LeftEye.SmoothedCoordinates.X == 0 && gazeData.LeftEye.SmoothedCoordinates.Y == 0) {
      Console.WriteLine("left eye close");
      NativeMethods.LeftClick();
    } else if(gazeData.RightEye.SmoothedCoordinates.X == 0 && gazeData.RightEye.SmoothedCoordinates.Y == 0) {
      Console.WriteLine("right eye close");
      NativeMethods.RightClick();
    } else if(screenX == 0 && screenY == 0) {
      Console.WriteLine("return in case of 0,0");
    } else {
      NativeMethods.SetCursorPos(screenX, screenY);
    }
  }

  public class NativeMethods {
    [System.Runtime.InteropServices.DllImportAttribute("user32.dll", EntryPoint = "SetCursorPos")]
    [return: System.Runtime.InteropServices.MarshalAsAttribute(System.Runtime.InteropServices.UnmanagedType.Bool)]
    public static extern bool SetCursorPos(int x, int y);

    [DllImport("user32.dll")]
    private static extern void SendInput(int nInputs, ref INPUT pInputs, int cbsize);

    public static void RightClick() {
      const int num = 2;
      INPUT[] inp = new INPUT[num];

      // マウスの右ボタンを押す
      inp[0].type = INPUT_MOUSE;
      inp[0].mi.dwFlags = MOUSEEVENTF_RIGHTDOWN;
      inp[0].mi.dx = 0;
      inp[0].mi.dy = 0;
      inp[0].mi.mouseData = 0;
      inp[0].mi.dwExtraInfo = 0;
      inp[0].mi.time = 0;

      // マウスの右ボタンを離す
      inp[1].type = INPUT_MOUSE;
      inp[1].mi.dwFlags = MOUSEEVENTF_RIGHTUP;
      inp[1].mi.dx = 0;
      inp[1].mi.dy = 0;
      inp[1].mi.mouseData = 0;
      inp[1].mi.dwExtraInfo = 0;
      inp[1].mi.time = 0;

      SendInput(num, ref inp[0], Marshal.SizeOf(inp[0]));
    }

    public static void LeftClick() {
      const int num = 2;
      INPUT[] inp = new INPUT[num];

      // マウスの左ボタンを押す
      inp[0].type = INPUT_MOUSE;
      inp[0].mi.dwFlags = MOUSEEVENTF_LEFTDOWN;
      inp[0].mi.dx = 0;
      inp[0].mi.dy = 0;
      inp[0].mi.mouseData = 0;
      inp[0].mi.dwExtraInfo = 0;
      inp[0].mi.time = 0;

      // マウスの左ボタンを離す
      inp[1].type = INPUT_MOUSE;
      inp[1].mi.dwFlags = MOUSEEVENTF_LEFTUP;
      inp[1].mi.dx = 0;
      inp[1].mi.dy = 0;
      inp[1].mi.mouseData = 0;
      inp[1].mi.dwExtraInfo = 0;
      inp[1].mi.time = 0;

      SendInput(num, ref inp[0], Marshal.SizeOf(inp[0]));
    }

    private const int INPUT_MOUSE = 0;                  // マウスイベント

    private const int MOUSEEVENTF_MOVE = 0x1;           // マウスを移動する
    private const int MOUSEEVENTF_ABSOLUTE = 0x8000;    // 絶対座標指定
    private const int MOUSEEVENTF_LEFTDOWN = 0x2;       // 左　ボタンを押す
    private const int MOUSEEVENTF_LEFTUP = 0x4;         // 左　ボタンを離す
    private const int MOUSEEVENTF_RIGHTDOWN = 0x8;      // 右　ボタンを押す
    private const int MOUSEEVENTF_RIGHTUP = 0x10;       // 右　ボタンを離す

    [StructLayout(LayoutKind.Explicit)]
    private struct INPUT {
      [FieldOffset(0)]
      public int type;
      [FieldOffset(4)]
      public MOUSEINPUT mi;
    };

    // マウスイベント(mouse_eventの引数と同様のデータ)
    [StructLayout(LayoutKind.Sequential)]
    private struct MOUSEINPUT {
      public int dx;
      public int dy;
      public int mouseData;
      public int dwFlags;
      public int time;
      public int dwExtraInfo;
    };
  }

  static void Main(string[] args) {
    GazeManager.Instance.Activate(GazeManager.ApiVersion.VERSION_1_0, GazeManager.ClientMode.Push);
    if(!GazeManager.Instance.IsConnected) {
      Console.WriteLine("EyeTribe Server has not been started");
    } else if(GazeManager.Instance.IsCalibrated) {
      Console.WriteLine(GazeManager.Instance.LastCalibrationResult);
      Console.WriteLine("Re-Calibrate");
    } else {
      Console.WriteLine("Start");
    }
    CursorControl cursor = new CursorControl(Screen.PrimaryScreen, true, true);
  }
}
```

## 感想
- the eye tribe server、デバイスを認識したりしなかったり、ストリーム来なかったり。Windows再起動するしかない
- Soomthされててもまだまだ動き回るマウスカーソル
- 公式では標準偏差使ってる？
- 移動平均とかも使うべきかも
- マウスポインタを見つめることでキャリブレーションを取る仕組みとか導入したいところ


## 参考

http://dev.theeyetribe.com/tutorial/
- 公式チュートリアル
- 2013/02/03現在、C# client versionのサンプルは間違い
- IGazeUpdateListenerはIGazeListenerに変更されてる？

http://romichi.ivory.ne.jp/wordpress/?p=739
- 先駆者
- C# client versionとPlain versionを混同している？

https://github.com/EyeTribe/tet-csharp-client
- 0.9.26 (2014-01-30)
- tet-csharp-client / TETCSharpClient / Data / GazeData.cs 

https://github.com/EyeTribe/tet-csharp-samples
- 0.9.26 (2014-01-30)
- tet-csharp-samples / TETControls / Cursor / CursorControl.cs

http://msdn.microsoft.com/ja-jp/library/cc411029.aspx
http://msdn.microsoft.com/ja-jp/library/cc411004.aspx
- Win32API