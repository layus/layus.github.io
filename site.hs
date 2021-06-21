--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll hiding (pandocCompiler)
import           Hakyll.Web.Sass (sassCompiler)

import           Data.ByteString.Lazy.UTF8 (toString, fromString)

import           Text.Pandoc.Definition     as Pandoc
import           Text.Pandoc.Walk           as Pandoc
import           Text.Pandoc.Extensions     as Pandoc
import           Text.Pandoc.Options        as Pandoc

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "favicon.ico" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*.scss" $ do
        route $ setExtension "css"
        let compressCssItem = fmap compressCss
        compile (compressCssItem <$> sassCompiler)

    match "css/*.css" $ do
        route   idRoute
        compile compressCssCompiler

    match "fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match (fromList ["about.md", "contact.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- Tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (postCtx tags)
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" (postCtx tags) (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx = mconcat 
                            [ listField "posts" (postCtx tags) (return posts)
                            , field "tags" (\_ -> renderTagList tags)
                            , defaultContext
                            ]

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    match "tikz/**.tikz" $ do
        route   $ gsubRoute "tikz/" (const "images/") `composeRoutes` setExtension ".png"
        compile $ getResourceString
            >>= loadAndApplyTemplate "templates/default.tikz" defaultContext
            >>= withItemBody (unixFilterLBS "bash" ["./scripts/pdflatex.sh"] . fromString)
            >>= withItemBody (unixFilterLBS "bash" ["-c", "pdfcrop -margins 10 - >(cat)"])
            -- >>= withItemBody (unixFilterLBS "convert" ["-density", "1200", "-trim", "PDF:-", "-quality", "100", "-flatten", "-sharpen", "0x1.0", "-resize", "25%", "PNG:-"])
            >>= withItemBody (unixFilterLBS "convert" ["-density", "1200", "-trim", "PDF:-", "-flatten", "-resample", "300", "PNG:-"])
            >>= withItemBody (unixFilterLBS "exiftool" ["-all=", "-"])


    -- Feeds
    create ["index.rss"] $ do
        route idRoute
        compile (feedCompiler "posts/*" renderRss "All posts")

    create ["index.atom"] $ do
        route idRoute
        compile (feedCompiler "posts/*" renderAtom "All posts")


    -- Post tags
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged '" <> tag <> "'"

        -- Copied from posts, need to refactor
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title <>
                        listField "posts" (postCtx tags) (return posts) <>
                        constField "tag" tag <>
                        defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

        -- Create RSS feed as well
        version "rss" $ do
            route   $ setExtension "rss"
            compile $ feedCompiler pattern renderRss title

        -- Create ATOM feed as well
        version "atom" $ do
            route   $ setExtension "atom"
            compile $ feedCompiler pattern renderAtom title

--------------------------------------------------------------------------------
postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ dateField "date" "%B %e, %Y"
    , tagsField "tags" tags
    , defaultContext
    ]

feedCtx :: Context String
feedCtx = mconcat
    [ bodyField "description"
    , Context $ \key -> case key of
        "title" -> unContext (mapContext escapeHtml defaultContext) key
        _       -> unContext mempty key
    , defaultContext
    ]

feedConfiguration :: String -> FeedConfiguration
feedConfiguration title = FeedConfiguration
    { feedTitle       = "Layus' short musings - " <> title
    , feedDescription = "A blog about build systems, Nix and everything"
    , feedAuthorName  = "layus"
    , feedAuthorEmail = "layus.on@gmail.com"
    , feedRoot        = "https://blog.layus.be"
    }

feedCompiler pattern render title = do
    posts <- fmap (take 10) . recentFirst =<<
        loadAllSnapshots pattern "content"
    render (feedConfiguration title) feedCtx posts

pandocReaderOptions = defaultHakyllReaderOptions
    { readerExtensions = Pandoc.enableExtension Pandoc.Ext_citations $ readerExtensions defaultHakyllReaderOptions
    , readerStripComments = True
    }
pandocWriterOptions = defaultHakyllWriterOptions

pandocCompiler = do
    --csl <- load "chicago.csl"
    --bib <- load "refs.bib"
   
    getResourceBody 
        >>= readPandocWith pandocReaderOptions
        >>= return . filter
        >>= return . writePandocWith pandocWriterOptions
    
  where
    filter = Pandoc.walk wrapCode

    wrapCode code@(Pandoc.CodeBlock _ _) = Pandoc.Div ("", ["scroll-wrapper"], []) [code]
    wrapCode x = x

--    match "tikz/**.tikz" $ version "1tex" $ do
--        route   $ setExtension ".tex"
--        compile $ getResourceString
--            >>= loadAndApplyTemplate "templates/default.tikz" defaultContext
--
--    match "tikz/**.tikz" $ version "2pdf" $ do
--        route   $ setExtension ".pdf"
--        compile $ getResourceString
--            >>= loadAndApplyTemplate "templates/default.tikz" defaultContext
--            >>= withItemBody (unixFilterLBS "bash" ["./scripts/pdflatex.sh"] . fromString)
--
--    match "tikz/**.tikz" $ version "3pdfcrop" $ do
--        route   $ setExtension ".pdfcrop"
--        compile $ getResourceString
--            >>= loadAndApplyTemplate "templates/default.tikz" defaultContext
--            >>= withItemBody (unixFilterLBS "bash" ["./scripts/pdflatex.sh"] . fromString)
--            >>= withItemBody (unixFilterLBS "bash" ["-c", "pdfcrop - >(cat)"])

