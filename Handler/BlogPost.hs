module Handler.BlogPost where

import Import
import Yesod.Form.Bootstrap3 (bfs,renderBootstrap3,BootstrapFormLayout(..))
import Yesod.Text.Markdown (markdownField)

blogPostForm :: AForm Handler BlogPost
blogPostForm = BlogPost 
        <$> areq textField (bfs ("Title"::Text)) Nothing
        <*> areq markdownField (bfs ("Article"::Text)) Nothing

getBlogPostNewR :: Handler Html
getBlogPostNewR = do
   (widget,encoding) <- generateFormPost $
                        renderBootstrap3 BootstrapBasicForm blogPostForm
   defaultLayout $ do
       $(widgetFile "blog/new")  

postBlogPostNewR :: Handler Html
postBlogPostNewR = do
   ((result,widget),encoding) <- runFormPost $
                                 renderBootstrap3 BootstrapBasicForm 
                                 blogPostForm
   case result of
        FormSuccess blogPost -> do
                                bId <- runDB $ insert blogPost
                                redirect (BlogPostR bId)
        _ -> defaultLayout $ do
                 $(widgetFile "blog/new")  

getBlogPostR :: BlogPostId -> Handler Html
getBlogPostR bpId = do
   blogPost <- runDB $ get404 bpId
   defaultLayout $ do
       $(widgetFile "blog/details")
