
class ChannelInfoObserver 
    attr_accessor :app

    def initialize(app)
        self.app = app
    end
 
    def is_some_change?(event)
        return false unless event&.chat&.type == 'channel'
        return false unless event&.new_chat_member&.status == 'administrator'
        true
    end

    def handle_chat_member(event)
        if is_some_change?(event)
            #TODO gather info about channel and current state of bot of it
        end

    end
    
end