#!/usr/bin/env ruby

return unless __FILE__ == $0

require_relative './tg-toolkit/src/autoload'
Autoloader.new.load_from(__dir__)

binding.irb
