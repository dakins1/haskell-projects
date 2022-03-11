cylinder r h = pi * r^^2 + pi * r^^2 + 2*pi*r

cylinder r h = 
      let top = pi * r^^2
          side = 2*pi*r*h
      in top + side + top

cylinder r h = top + side + top 
  where top = pi * r^^2
        side = 2*pi*r*h

cp ~sfogarty/.vimrc ~/:
