#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE OverlappingInstances #-}
#define __OVERLAPS__
#define __OVERLAPPABLE__
#define __INCOHERENT__
#else
#define __OVERLAPS__ {-# OVERLAPS #-}
#define __OVERLAPPABLE__ {-# OVERLAPPABLE #-}
#define __INCOHERENT__ {-# INCOHERENT #-}
#endif
