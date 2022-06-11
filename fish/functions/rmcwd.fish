function rmcwd
set -l cwd (pwd)
pause "$cwd を全削除します"
cd -
rm -rf "$cwd"
end
