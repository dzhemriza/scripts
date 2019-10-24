echo "My first PS script...";

$count = 1..10;
$prefix = Get-Date -Format "yyyy-dd-mm-hh-mm-ss";

$path = "c:\tmp\remove-me-mate-this-is-a-test";

New-Item -ItemType Directory -Force -Path $path;

$count | ForEach-Object {
$fileName = "$path\random_$prefix-$_.r";
echo "File - $fileName";
$out = new-object byte[] 1048576;
(new-object Random).NextBytes($out);
[IO.File]::WriteAllBytes($fileName, $out);
}

echo "My first PS script...done.";

