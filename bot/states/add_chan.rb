
class AddChannelState < BaseState 

    def run 
        code = wait_for_linking()
        name = capture_text_or_cancel(
            "Add this bot to admins (only `post messages` right needed) in your channel and post this code: #{code}", 
            "Cancel")

        unless name  
            forget_linking()
            main_menu()
        end
        
        main_menu()

    end

    private
    
    def main_menu() 
        switch_state MainMenuState.new
    end

end


class ContinueAddingChannelState < BaseState

    attr_accessor :chan_id

    def initialize(chan_id)
        @chan_id = chan_id
    end

    def run 
        say "You linked with channel: #{ Channel.find_by(chat_id: chan_id).title }"
        switch_state CategorySettingsState.new()
    end
end


class CategorySettingsState < BaseState

    def run 
        suggest_it("Select channel category")
            .option("Personal blog") do end 
            .option("Entertaining") do end
            .option("News") do end 
            .option("18+ ") do end
            .option("Other") do end 
            .exec()

        switch_state PostinSettingsState.new
    end

end

class PostinSettingsState < BaseState 

    def run 
        setup_posting_settings()
        switch_state MainMenuState.new
    end

    def restart() 
        switch_state __clean_state(self) 
    end

    def by_time_settings()
        suggest_it("How often you ready to post adverts ?")
            .option("Every 12 hours") do end
            .option("Once a day") do end
            .option("Once a week") do end 
            .option("Cancel") do restart() end 
            .exec()
    end

    def by_post_count() 
        suggest_it("After how much posts you ready to post ?")
            .option("Every 3 posts") do end 
            .option("Every 10 posts") do end 
            .option("Every 20 posts") do end 
            .option("Cancel") do restart() end 
            .exec()
    end

    def setup_posting_settings()
        suggest_it("How to post adverts in this channel ?")
            .option("By time") do 
                by_time_settings()
            end
            .option("By post count") do
                by_post_count()
            end 
            .option("By request") do
            end
            .exec() 
    end


end

