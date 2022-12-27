



class PostSettings < BaseState
    

    def initialize(back) 
        @back = __clean_state(back) 
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


    def run 
        setup_posting_settings()
        switch_state @back
    end

end

class AddChannelState < BaseState 
 

    def run 
        code = wait_for_linking()

        say "Add this bot to admins (only `post messages` right needed) in your channel and post this code: #{code}"

        name = capture_text_or_cancel("Enter channel name", "Cancel")

        main_menu() unless name  

        category = suggest_it("Select channel category")
            .option("Personal blog") do end 
            .option("Entertaining") do end
            .option("News") do end 
            .option("18+ ") do end
            .option("Other") do end 
            .exec()

        say "Link channel with bot: todo"

        switch_state(
            PostSettings.new(
                MainMenuState.new))
    end

    private
    
  

    def main_menu() 
        switch_state MainMenuState.new
    end
end
