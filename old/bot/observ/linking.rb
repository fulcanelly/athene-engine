

module StateExtension

    #returns test for linking 
    def wait_for_linking() 
        escape do |ctx| 
            ctx.global.linking_observer.sub(ctx.extra.user_id) 
        end
    end
    
    def forget_linking() 
        escape do |ctx| 
            ctx.global.linking_observer.unsub(ctx.extra.user_id) 
        end
    end

end

#TODO move to model folder
class UserCode < ActiveRecord::Base
end

class ChannelLinkingMigration < ActiveRecord::Migration[7.0]
    def change
        create_table :user_codes, id: :uuid, if_not_exists: true do |t|
            t.bigint :user_id
            t.timestamps
        end

    end
end

class ChannelLinkingObserver 
    attr_accessor :app

    def initialize(app)
        @app = app
    end

    def setup()
        #creating table 
        ChannelLinkingMigration.new.change 
        #setting up refernce to self 
        app.provider.global_ctx.linking_observer = self
    end

    def sub(user_id)
        UserCode.create(user_id:).id
    end

    def unsub(user_id) 
        UserCode.find_by(user_id:)
            .destroy()
    end

    def handle_channel_post(event)
        chan_id = event.chat.id 
        text = event.text  

        UserCode.find_by(id: text)
            &.tap do |user_code|
                user_id = user_code.user_id 
                _notify(chan_id, user_id)
                user_code.destroy()
            end
    end

    def _notify(chan_id, user_id)
        TgSwitchStateAction.new(LinkingDoneState.new(chan_id))
            .exec(app.provider.find_by_user_id(user_id))
    end

end

class LinkingDoneState < BaseState

    attr_accessor :chan_id 

    def initialize(chan_id)
        @chan_id = chan_id
    end

    #TODO
    def run 
        me = myself
        escape do 
            me.update(
                channels: [
                    *me.channels, 
                    Channel.find_by(chat_id: chan_id)
                ].uniq do _1.chat_id end 
            )
        end

        switch_state ContinueAddingChannelState.new(chan_id)

    end

end

