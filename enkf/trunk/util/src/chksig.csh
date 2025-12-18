set analdate=1999091500
set datapath=/scratch2/scratchdirs/whitaker/gfsenkf_t62_1999iau
set nanal=1
set nanals=64
set fhr=03
while ($nanal <= $nanals)
  set charnanal="mem`printf %03i $nanal`"
  echo $charnanal
  ./chksig.x $datapath/$analdate/sfg_${analdate}_fhr${fhr}_${charnanal}
  @ nanal = $nanal + 1
end
