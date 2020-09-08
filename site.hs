--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll hiding (pandocCompiler)
import           Hakyll.Web.Sass (sassCompiler)

import           Data.ByteString.Lazy.UTF8 (toString, fromString)

import           Text.Pandoc.Definition     as Pandoc
import           Text.Pandoc.Walk           as Pandoc

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

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
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
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

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

    create ["rss.xml"] $ do
        route idRoute
        compile (feedCompiler renderRss)

    create ["atom.xml"] $ do
        route idRoute
        compile (feedCompiler renderAtom)

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Layus' short musings"
    , feedDescription = "A blog about build systems, Nix and everything"
    , feedAuthorName  = "layus"
    , feedAuthorEmail = "layus.on@gmail.com"
    , feedRoot        = "http://layus.github.io"
    }

feedCompiler render = do
    let feedCtx = postCtx <> bodyField "description"
    posts <- fmap (take 10) . recentFirst =<<
        loadAllSnapshots "posts/*" "content"
    render myFeedConfiguration feedCtx posts

pandocCompiler = pandocCompilerWithTransform defaultHakyllReaderOptions defaultHakyllWriterOptions filter
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

