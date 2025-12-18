program readozclim 
 open(9,file="../fix_am/ozone.clim",form="unformatted")
 read (9) latsozc, levozc, timeozc, blatc4
 print *,latsozc, levozc, timeozc, blatc4
end program readozclim
