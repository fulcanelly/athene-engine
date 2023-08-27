require 'active_record'
require 'ostruct'
require 'net/http'
require 'json'
require 'cgi'
require 'open-uri'
require 'recursive-open-struct'
require 'logger'
require 'colored'
require 'awesome_print'
require 'faker'

require_relative './tg-toolkit/src/autoload'


Autoloader.new.load_from(__dir__)

token = ENV['TG_TOKEN']

raise 'env variable TG_TOKEN required' unless token
raise 'env variable TG_TOKEN required' if token.empty?

pp Config

file_lister = proc do
    list_all_rb_files()
end


class MyApplication < Application

    def setup_handlers()
        chan_observer = ChannelInfoObserver.new(self)

        link_observer = ChannelLinkingObserver.new(self)
        link_observer.setup()

        self.pipe.on_my_chat_member do |event|
            ap event
            chan_observer.handle_chat_member(event)
        end

        self.pipe.on_channel_post do |event|
            ap event
            link_observer.handle_channel_post(event)
        end

        super()
    end

end

CreateAll.new.change

HotReloader.new(file_lister).tap do |reloader|
    reloader.init
    reloader.entry_point do

        bot = Bot.new(token)
        pipe = EventPipe.new
        provider = ContextProvider.new(bot, StartingState)

        bot.connect

        MyApplication.new(bot, pipe, provider)
            .tap do |app|
                app.setup_handlers()
            #   app.run_ctxes()
                app.run()
            end
    end
    reloader.start
end



