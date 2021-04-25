m2libkey=`grep -E "M2Lib__[0-9A-F]{12}" m2lib.key | sed -E -e "s/^.*(M2Lib__[0-9A-F]{12}).*$/\1/"`
storagekey=`grep -E "Storage__[0-9A-F]{12}" storage.key | sed -E -e "s/^.*(Storage__[0-9A-F]{12}).*$/\1/"`

echo "M2Lib key: ${m2libkey}"
echo "Storage key: ${storagekey}"

sed -E -e "s/(^.*)M2Lib__[0-9A-F]{12}(.*$)/\1${m2libkey}\2/" asm/m2lib.asm > asm/m2lib.asmk
sed -E -e "s/(^.*)Storage__[0-9A-F]{12}(.*$)/\1${storagekey}\2/" asm/storage.asm > asm/storage.asmk

mv asm/m2lib.asmk asm/m2lib.asm
mv asm/storage.asmk asm/storage.asm

echo "Both m2lib.asm and storage.asm updated."
