require 'active_record'
require 'ostruct'
require 'net/http'
require 'json'
require 'cgi'
require 'open-uri'
require 'recursive-open-struct'
require 'logger'
require 'colored'

require_relative './tg-toolkit/src/autoload'


Autoloader.new.load_from(__dir__)

token = ENV['TG_TOKEN']

raise 'env variable TG_TOKEN required' unless token 
raise 'env variable TG_TOKEN required' if token.empty?

pp Config

file_lister = proc do 
    list_all_rb_files() 
end

HotReloader.new(file_lister).tap do |reloader|
    reloader.init
    reloader.entry_point do 

        bot = Bot.new(token)
        pipe = EventPipe.new 
        provider = ContextProvider.new(bot, StartingState)

        bot.connect

        Application.new(bot, pipe, provider)
            .tap do |app|
                app.setup_handlers()
            #   app.run_ctxes()
                app.run()
            end
    end
    reloader.start
end


bot = Bot.new(token)

TgToolkit.new(
   bot, EventPipe.new,  ContextProvider.new(bot, StartingState)) 
   .tap do 
        _1.setup
        _1.start
    end
    
