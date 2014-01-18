# TODO: Make this work at least as nicely as the simple prompt in irb
Pry.config.prompt = [
  proc { '>> ' },
  proc { '...' }
]
