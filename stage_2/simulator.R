library(warfkit)

args <- commandArgs(T)

load(args[1]) 

avatars = Map(process_avatar, avatars) 

write(avatars, file=args[2])
