# TODO move all ActiveRecord classes to module Models

class User < ActiveRecord::Base 
    has_many :channel_ownerships
    has_many :channels, through: :channel_ownerships
end

class ChannelOwnership < ActiveRecord::Base 
    belongs_to :user
    belongs_to :channel
end

class Channel < ActiveRecord::Base 
    has_many :channel_ownership
    has_many :users, through: :channel_ownership

    has_many :channel_properties

    def admins 
        users 
    end

end

class ChannelProperty < ActiveRecord::Base 
    belongs_to :channel
end


#TODO move all mirgations to Migration module
class CreateChannelMigration < ActiveRecord::Migration[7.0]
    def change 

        create_table :channel_ownerships, if_not_exists: true do |t|
            t.belongs_to :user 
            t.belongs_to :channel
    
            t.timestamps 
        end
    
        create_table :channels, if_not_exists: true do |t|
            t.bigint :chat_id

            t.string :my_status 
            t.string :title 

            t.string :username
            t.string :join_link
    
            t.timestamps 
        end
    
        create_table :channel_properties, if_not_exists: true do |t| 
            t.string :key 
            t.string :value
            t.belongs_to :channel
        end
  
    end
end

CreateChannelMigration.new.change


class ChannelInfoObserver 
    attr_accessor :app

    def initialize(app)
        self.app = app
    end
 
    def is_right_event?(event)
        return false unless event&.chat&.type == 'channel'
        true
    end

    def _create_or_update_info(event)
        chat_id = event.chat.id 

        entry = {
            chat_id: chat_id,
            username: event.chat.username,
            title: event.chat.title,
            my_status: event.new_chat_member.status, #WARN is it always bot's status ?
        }

        chan = Channel.find_by(chat_id: chat_id) 
        if chan then 
            chan.update(**entry)
        else 
            Channel.create(**entry)
        end
    end

    def handle_chat_member(event)
        return unless is_right_event?(event)
        _create_or_update_info(event)
    end
    
end