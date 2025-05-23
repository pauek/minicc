def posicio_maxim(v, m):
  pos_max = 0
  pos = 0
  while(pos <= m):
    if (v[pos] > v[pos_max]):
      pos_max = pos
    pos += 1
  return(pos_max)