set analdate=2000010100
set datapath=/scratch1/scratchdirs/whitaker/gfsenkf_t126_1999iau
set nanal=1
set nanals=64
set fhr=03
while ($nanal <= $nanals)
  set charnanal="mem`printf %03i $nanal`"
  echo $charnanal
  ./chksfc.x $datapath/$analdate/bfg_${analdate}_fhr${fhr}_${charnanal}
  @ nanal = $nanal + 1
end
